module type PfdType =
  sig
    type delivarable
    type activity
    type member
    val compare_delivarable : delivarable -> delivarable -> int
    val compare_activity : activity -> activity -> int
    val compare_member : member -> member -> int
  end

module Make(Pfd: PfdType) =
  struct

    type time = int

    module DSet =
      Set.Make
        (struct
          type t = Pfd.delivarable
          let compare = Pfd.compare_delivarable
        end)

    module ASet =
      Set.Make
        (struct
          type t = Pfd.activity
          let compare = Pfd.compare_activity
        end)

    module AMap =
      Map.Make
        (struct
          type t = Pfd.activity
          let compare = Pfd.compare_activity
        end)

    module MSet =
      Set.Make
        (struct
          type t = Pfd.member
          let compare = Pfd.compare_member
        end)

    module MMap =
      Map.Make
        (struct
          type t = Pfd.member
          let compare = Pfd.compare_member
        end)

    type act_mem = Pfd.activity * MSet.t

    let compare_act_mem (act0, mset0) (act1, mset1) =
      let s = Pfd.compare_activity act0 act1 in
      if s <> 0 then
        s
      else
        MSet.compare mset0 mset1

    module AMSet =
      Set.Make
        (struct
          type t = act_mem
          let compare = compare_act_mem
        end)

    module AMMap =
      Map.Make
        (struct
          type t = act_mem
          let compare = compare_act_mem
        end)

    module MSetSet =
      Set.Make
        (struct
          type t = MSet.t
          let compare = MSet.compare
        end)

    type state = {
        t : time;
        r : time AMap.t;
        path : (time * time * AMSet.t) list;
      }

    exception Error of string
    exception Dfs_timeout of state
    exception Activity_not_found of Pfd.activity

    let error s = raise (Error s)

    let rec list_power xs =
      match xs with
        [] -> [[]]
      | x::xs' ->
         let yss = list_power xs' in
         yss @ (List.map (fun ys -> x::ys) yss)

    let msetset_power (s : MSet.t) =
      let xs = MSet.fold (fun x xs -> x::xs) s [] in
      let xss = list_power xs in
      MSetSet.of_list (List.map MSet.of_list xss)

    let sort_r (r : AMSet.t list) =
      let s a =
        AMSet.fold
          (fun (_, m) acc -> acc + MSet.cardinal m)
          a 0
      in
      let c a b = s b - s a in
      List.sort c r

    let sched
          (timeout : time)
          (in_set : DSet.t)
          (acts : ASet.t)
          (assign : Pfd.activity -> MSet.t)
          (inp : Pfd.activity -> DSet.t)
          (out : Pfd.activity -> DSet.t)
          (sync : Pfd.activity -> bool)
          (map_time : time AMap.t)
        : (time * time * AMSet.t) list =

      let delivs =
        ASet.fold
          (fun a delivs ->
            DSet.union (DSet.union delivs (inp a)) (out a))
          acts DSet.empty
      in

      let time a =
        match AMap.find_opt a map_time with
          Some x -> x
        | None -> raise (Activity_not_found a) in

      let rec maxmal_subsets ms q =
        if AMSet.is_empty q then
          error "maxmal_subsets: q is empty"
        else
          let (a, m) = AMSet.choose q in
          let q1 = AMSet.remove (a, m) q in
          if AMSet.is_empty q1 then
            [AMSet.singleton (a, m)]
          else
            let r1 =
              let ms' = MSet.union ms m in
              let q2 =
                AMSet.filter
                  (fun (a', m) -> a' <> a && MSet.is_empty (MSet.inter m ms'))
                  q1
              in
              if AMSet.is_empty q2 then
                [AMSet.singleton (a, m)]
              else
                let r = maxmal_subsets ms' q2 in
                List.map (fun x -> AMSet.add (a, m) x) r
            in
            let r2 = maxmal_subsets ms q1 in
            r1 @ r2
      in

      let rest s a =
        match AMap.find_opt a s.r with
          Some t -> t
        | None -> time a
      in

      let find_min s ams =
        if AMSet.is_empty ams then
          error "find_min"
        else
          let (a, _) = AMSet.choose ams in
          AMSet.fold
            (fun (a, m) d ->
              let r = rest s a in
              if sync a then
                min d r
              else
                let c = MSet.cardinal m in
                min d ((r + c - 1) / c))
            ams (rest s a)
      in

      let next (s : state) =
        let da = ASet.filter (fun a -> rest s a = 0) acts in
        let dd =
          DSet.union
            in_set
            (DSet.filter
               (fun d ->
                 ASet.exists (fun a -> DSet.mem d (out a)) da)
               delivs)
        in
        let p =
          ASet.filter
            (fun a ->
              DSet.subset (inp a) dd && not (ASet.mem a da))
            acts
        in
        if ASet.is_empty p then
          []
        else
          let q =
            ASet.fold
              (fun a q ->
                let m = assign a in
                if sync a then
                  AMSet.add (a, m) q
                else
                  let pm = msetset_power m in
                  MSetSet.fold
                    (fun m q ->
                      if MSet.is_empty m then q else
                        AMSet.add (a, m) q)
                    pm q)
              p AMSet.empty
          in
          let r = maxmal_subsets MSet.empty q in
          let r = sort_r r in
          List.map
            (fun ams ->
              let d = find_min s ams in
              let r =
                AMSet.fold
                  (fun (a, m) r ->
                    let x = AMap.find a r in
                    if sync a then
                      AMap.add a (x - d) r
                    else
                      let c = MSet.cardinal m in
                      AMap.add a (max (x - d * c) 0) r)            
                  ams s.r
              in
              { t = s.t + d; r; path = (s.t, s.t + d, ams)::s.path })
            r
      in

      let start = Unix.time () in

      let rec dfs m s =
        let ns = next s in
        if ns = [] then
          begin
            if start +. float timeout < Unix.time () then
              let s = if m.t <= s.t then m else s in
              raise (Dfs_timeout s)
            else
              s
          end
        else
          let rec loop m ns =
            match ns with
              [] -> m
            | s::ns' ->
               if s.t >= m.t then
                 begin
                   if start +. float timeout < Unix.time () then
                     raise (Dfs_timeout m)
                   else
                     loop m ns'
                 end
               else
                 let s = dfs m s in
                 if s.t < m.t then
                   loop s ns'
                 else
                   loop m ns'
          in
          loop m ns
      in
      try
        let s =
          dfs { t = 10000000000; r = map_time; path = [] }
            { t = 0; r = map_time; path = [] }
        in
        s.path
      with
        Dfs_timeout s -> s.path

  end
