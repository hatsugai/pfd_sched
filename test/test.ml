type deliv = D of int
[@@deriving show { with_path = false }, eq, ord]
type act = A of int
[@@deriving show { with_path = false }, eq, ord]
type mem = M of int
[@@deriving show { with_path = false }, eq, ord]

open PfdSched.Sched

module X =
  Make
    (struct
      type delivarable = deliv
      type activity = act
      type member = mem
      let compare_delivarable = compare_deliv
      let compare_activity = compare_act
      let compare_member = compare_mem
    end)

open X

let in_set = DSet.of_list [D 0; D 1; D 2]

let acts = ASet.of_list [A 0; A 1; A 2; A 3; A 4; A 5]

let assign a = MSet.of_list [M 0; M 1]

let inp a =
  match a with
    A 0 -> DSet.of_list [D 0]
  | A 1 -> DSet.of_list [D 1]
  | A 2 -> DSet.of_list [D 2]
  | A 3 -> DSet.of_list [D 3]
  | A 4 -> DSet.of_list [D 4]
  | A 5 -> DSet.of_list [D 5]
  | _ -> raise (Error "inp")

let out a =
  match a with
    A 0 -> DSet.of_list [D 3]
  | A 1 -> DSet.of_list [D 4]
  | A 2 -> DSet.of_list [D 5]
  | A 3 -> DSet.of_list [D 6]
  | A 4 -> DSet.of_list [D 7]
  | A 5 -> DSet.of_list [D 8]
  | _ -> raise (Error "inp")

let sync a = false

let map_time =
  List.fold_left
    (fun amap (act, time) -> AMap.add act time amap)
    AMap.empty
    [(A 0, 10);
     (A 1, 10);
     (A 2, 10);
     (A 3, 10);
     (A 4, 10);
     (A 5, 10)]

let path = sched 60 in_set acts assign inp out sync map_time

open Printf

let show_mset ms =
  let s = 
    MSet.fold
      (fun m s -> sprintf "%s %s;" s (show_mem m))
      ms "{"
  in s ^ " }"

let show_amset ams =
  let s =
    AMSet.fold
      (fun (act, ms) s ->
        sprintf "%s (%s, %s);" s (show_act act) (show_mset ms))
      ams ""
  in s ^ " }"

let print_path path =
  let p i (t0, t1, ams) =
    printf "%d. %d-%d %s\n" i t0 t1 (show_amset ams)
  in
  printf "--- path ---\n";
  List.iteri p (List.rev path);
  printf "------------\n"

let () = print_path path
