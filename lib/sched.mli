module type PfdType =
  sig
    type delivarable
    type activity
    type member
    val compare_delivarable : delivarable -> delivarable -> int
    val compare_activity : activity -> activity -> int
    val compare_member : member -> member -> int
  end

module Make :
  functor (Pfd : PfdType) ->
    sig
      type time = int
      module DSet : Set.S with type elt = Pfd.delivarable
      module ASet : Set.S with type elt = Pfd.activity
      module AMap : Map.S with type key = Pfd.activity
      module MSet : Set.S with type elt = Pfd.member
      type act_mem = Pfd.activity * MSet.t
      module AMSet : Set.S with type elt = act_mem

      exception Error of string
      exception Activity_not_found of Pfd.activity

      val sched :
        time ->
        DSet.t ->
        ASet.t ->
        (Pfd.activity -> MSet.t) ->
        (Pfd.activity -> DSet.t) ->
        (Pfd.activity -> DSet.t) ->
        (Pfd.activity -> bool) ->
        time AMap.t ->
        (time * time * AMSet.t) list
    end
