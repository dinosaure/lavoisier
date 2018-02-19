module Endianess:
sig
  module type S =
  sig
    type mask = int

    val branching_bit: int -> int -> mask
    val mask: int -> mask -> int
    val shorter: mask -> mask -> bool
  end

  module Little: S
  module Big: S
end

module type S =
sig
  type key = int

  type +'a t

  val empty: 'a t
  val lookup: key -> 'a t -> 'a
  val find: key -> 'a t -> 'a
  val add: key -> 'a -> 'a t -> 'a t

  exception Unchanged

  val strict_add: key -> 'a -> 'a t -> 'a t

  type 'a decision = 'a -> 'a -> 'a

  val fine_add: 'a decision -> key -> 'a -> 'a t -> 'a t
  val mem: key -> 'a t -> bool
  val singleton: key -> 'a -> 'a t
  val is_empty: 'a t -> bool
  val is_singleton: 'a t -> (key * 'a) option
  val cardinal: 'a t -> int
  val choose: 'a t -> key * 'a
  val pop: key -> 'a t -> ('a * 'a t)
  val remove: key -> 'a t -> 'a t
  val union: 'a t -> 'a t -> 'a t
  val fine_union: 'a decision -> 'a t -> 'a t -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_rev: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map: ('a -> 'b) -> 'a t -> 'b t
  val endo_map: ('a -> 'a) -> 'a t -> 'a t
  val pp: int Fmt.t -> 'a Fmt.t -> 'a t Fmt.t
end

module Make (X: Endianess.S): S

module Little: S
module Big: S
