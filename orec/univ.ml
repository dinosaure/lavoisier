module Eq =
struct

  type (_, _) refl = Refl : ('a, 'a) refl

  external identity : 'a -> 'b = "%identity"
  let cast : type a b. (a, b) refl -> a -> b = fun Refl -> identity
end

module type X =
sig

  type t

  val hash: int -> t
end

module type S =
sig

  type hash
  type 'a t
  type key

  val hash: 'a t -> 'a -> hash
  val create: name:string -> ('a -> hash) -> 'a t
  val key: 'a t -> key
  val int: 'a t -> int
  val name: 'a t -> string
  val same: 'a t -> 'b t -> bool
  val equal: 'a t -> 'b t -> ('a, 'b) Eq.refl option
  val equal_exn: 'a t -> 'b t -> ('a, 'b) Eq.refl

  type binding

  val binding: 'a t -> 'a -> binding
  val extract: 'a t -> binding -> 'a option
  val extract_exn: 'a t -> binding -> 'a
end

module Make (Hash: X): S with type hash = Hash.t =
struct

  type hash = Hash.t
  type key = extension_constructor

  module Witness =
  struct

    module Key =
    struct
      type _ t = ..

      let hash _ t =
        Hash.hash Obj.(extension_id (extension_constructor t))
    end

    module type S =
    sig
      type t
      type _ Key.t += Key: t Key.t
    end

    type 'a t = (module S with type t = 'a)

    let ok = Some Eq.Refl

    let equal (type u) (type v)
      : u t -> v t -> (u, v) Eq.refl option
      = fun (module U) (module V) ->
        match U.Key with
        | V.Key -> ok
        | _ -> None

    let ( =? ) = equal

    let equal_exn a b =
      match equal a b with
      | Some (_ as refl) -> refl
      | None -> invalid_arg "equal_exn"

    let ( =! ) = equal_exn

    let hash (type a) hash (module M: S with type t = a) =
      Key.hash hash M.Key

    let create (type u) () =
      let module K = struct
        type t = u
        type _ Key.t += Key: t Key.t
      end in (module K: S with type t = u)

    let key (type u) (module M: S with type t = u) : extension_constructor =
      [%extension_constructor M.Key]

    let int key : int =
      Obj.extension_id key
  end

  type 'a t =
    { witness : 'a Witness.t
    ; name    : string
    ; hash    : 'a -> hash }

  let equal a b = Witness.equal a.witness b.witness
  let equal_exn a b = Witness.equal_exn a.witness b.witness

  let hash : 'a t -> 'a -> hash = fun x -> x.hash

  let create ~name hash =
    { witness = Witness.create ()
    ; name
    ; hash }

  let key { witness; _ } = Witness.key witness
  let int t = Witness.(int @@ key t.witness)
  let name { name; _ } = name

  let same a b =
    match Witness.equal a.witness b.witness with
    | Some _ -> true
    | None -> false

  type binding = B : 'a Witness.t * 'a -> binding

  let binding
    : type a. a t -> a -> binding
    = fun univ v -> B (univ.witness, v)

  let extract
    : type a. a t -> binding -> a option
    = fun t (B (w', a)) ->
      let open Witness in match t.witness =? w' with
      | Some Eq.Refl -> Some a
      | None -> None

  let extract_exn
    : type a. a t -> binding -> a
    = fun t (B (w', a)) ->
      let open Witness in match t.witness =! w' with
      | Eq.Refl -> a
end

module Default = Make(struct type t = int external hash : int -> t = "%identity" end)
