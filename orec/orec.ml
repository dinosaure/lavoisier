module type S =
sig
  type hash

  module Univ : Univ.S with type hash := hash

  type t

  type 'a field_instr

  type 'info get = (('a, 'm) Data.getter * 'res) field_instr
    constraint 'info = < x : 'a; mut : 'm; ret : 'res >
  type 'a field = < x : 'a; mut : Data.imm; ret : 'a option > get
  type 'a mutable_field = < x : 'a; mut : Data.mut; ret : 'a option > get
  type 'a exn_field = < x : 'a; mut : Data.imm; ret : 'a > get
  type 'a exn_mutable_field = < x : 'a; mut : Data.mut; ret : 'a > get

  type ('param, 't) update = ('param Data.updater * 't) field_instr

  val empty: t
  val new_field: name:string -> ('a -> hash) -> 'a field
  val new_field_mutable: name:string -> ('a ref -> hash) -> 'a mutable_field
  val new_field_exn: name:string -> ('a -> hash) -> 'a exn_field
  val new_field_mutable_exn: name:string -> ('a ref -> hash) -> 'a exn_mutable_field

  val put: < x : 'ty; .. > get -> 'ty -> (_ Data.const, t) update
  val fmap: < x : 'ty; .. > get -> ('ty -> 'ty) -> ('a Data.fn, t) update
  val and_then: (Data.any, t) update -> (Data.any, t) update -> (Data.any, t) update
  val copy: < x : 'ty; mut : Data.mut; .. > get -> ('a Data.fn, t) update
  val delete: < .. > get -> ('a Data.del, t) update

  val ( ^= ): < x : 'ty; .. > get -> 'ty -> (_ Data.const, t) update
  val ( |= ): < x : 'ty; .. > get -> ('ty -> 'ty) -> ('a Data.fn, t) update
  val ( & ): (Data.any, t) update -> (Data.any, t) update -> (Data.any, t) update

  val get: < ret : 'ret; .. > get -> t -> 'ret
  val update: (Data.any, t) update -> t -> t
  val set: < x : 'ty; mut : Data.mut; .. > get -> 'ty -> t -> unit

  val (.%{}): t -> (_ * 'ret) field_instr -> 'ret
  val (.%{}<-): t -> < x : 'ty; mut : Data.mut; .. > get -> 'ty -> unit

  val transmute:
    (< x : 'ty; mut : 'm; .. > as 'x) get
    -> ('ty, 'rt) Bijection.t
    -> < x : 'rt; mut : 'm; ret : 'rt option > get

  val ( @: ):
    (< x : 'ty; mut : 'm; .. > as 'x) get
    -> ('ty, 'rt) Bijection.t
    -> < x : 'rt; mut : 'm; ret : 'rt option > get

  val transmute_exn:
    (< x : 'ty; mut : 'm; .. > as 'x) get
    -> ('ty, 'rt) Bijection.t
    -> < x : 'rt; mut : 'm; ret : 'rt > get

  val ( @:! ):
    (< x : 'ty; mut : 'm; .. > as 'x) get
    -> ('ty, 'rt) Bijection.t
    -> < x : 'rt; mut : 'm; ret : 'rt > get
end

module Make (Hash: Univ.X): S with type hash = Hash.t =
struct
  type hash = Hash.t

  module Univ = Univ.Make(Hash)
  module Map = Patricia.Little

  type 'data key =
    { witness : 'fy Univ.t
    ; storage : ('ty, 'fy, 'm) Data.storage
    ; access  : ('ty, 'rt) Data.access }
    constraint 'data = < mut : 'm; typ : 'ty; access : 'rt; stored : 'fy >

  type t = Univ.binding Map.t

  let empty : t = Map.empty

  let find_exn univ r =
    Map.find (Univ.int univ) r
    |> Univ.extract_exn univ

  let find univ r =
    match find_exn univ r with
    | x -> Some x
    | exception Not_found -> None

  let find_generator
    : type ty tr. (ty, tr) Data.access -> ty Univ.t -> t -> tr
    = fun access univ r ->
      match access with
      | Data.Exn -> find_exn univ r
      | Data.Opt -> find univ r

  let add univ v r =
    Map.add (Univ.int univ) (Univ.binding univ v) r

  let delete_key key r =
    Map.remove (Univ.int key.witness) r

  type 'info field_instr =
    | Get :
        < typ : 'ty; access : 'rt; mut : 'm; .. > key
      -> (('ty, 'm) Data.getter * 'rt) field_instr
    | Indirect_get :
        < typ : 'ty; mut : 'm; .. > key * ('ty, 'x) Bijection.t * ('x, 't) Data.access
      -> (('x, 'm) Data.getter * 't) field_instr
    | Update :
        < typ : 'ty; .. > key * 'ty
      -> ('a Data.const Data.updater * t) field_instr
    | Fn_update :
        < typ : 'ty; .. > key * ('ty -> 'ty)
      -> ('a Data.fn Data.updater * t) field_instr
    | And :
        ('any Data.updater * t) field_instr * ('any Data.updater * t) field_instr
      -> ('any Data.updater * t) field_instr
    | Delete :
        < .. > key -> ('a Data.del Data.updater * t) field_instr

  type 'info get = (('a, 'm) Data.getter * 'res) field_instr
    constraint 'info = < x : 'a; mut : 'm; ret : 'res >
  type 'a field = < x : 'a; mut : Data.imm; ret : 'a option > get
  type 'a mutable_field = < x : 'a; mut : Data.mut; ret : 'a option > get
  type 'a exn_field = < x : 'a; mut : Data.imm; ret : 'a > get
  type 'a exn_mutable_field = < x : 'a; mut : Data.mut; ret : 'a > get

  type ('param, 't) update = ('param Data.updater * 't) field_instr

  let new_field_generic =
    fun ~name hasher storage access ->
      Get { witness = Univ.create ~name hasher
          ; storage
          ; access; }

  let new_field ~name hasher = new_field_generic ~name hasher Data.Imm Data.Opt
  let new_field_mutable ~name hasher = new_field_generic ~name hasher Data.Mut Data.Opt
  let new_field_exn ~name hasher = new_field_generic ~name hasher Data.Imm Data.Exn
  let new_field_mutable_exn ~name hasher = new_field_generic ~name hasher Data.Mut Data.Exn

  let put
    : type ty m ret. < x : ty; mut : m; ret : ret > get -> ty -> ('a Data.const, t) update
    = fun instr x -> match instr with
      | Get key -> Update (key, x)
      | Indirect_get (key, c, access) ->
        Update (key, c.Bijection.of_ x)

  let ( ^= ) field x = put field x

  let fmap
    : type ty m ret. < x : ty; mut : m; ret : ret > get -> (ty -> ty) -> ('a Data.fn, t) update
    = fun instr f -> match instr with
      | Get key -> Fn_update (key, f)
      | Indirect_get (key, c, access) ->
        Fn_update (key, fun x -> x |> c.Bijection.to_ |> f |> c.Bijection.of_)

  let ( |= ) field f = fmap field f

  let copy field = field |= (fun x -> x)

  let delete = function
    | Get key -> Delete key
    | Indirect_get (key, _, _) -> Delete key

  let deref
    : type ty fy brand. (ty, fy, brand) Data.storage -> fy -> ty
    = fun storage v -> match storage with
      | Data.Mut -> !v
      | Data.Imm -> v

  let ref'
    : type ty fy brand. (ty, fy, brand) Data.storage -> ty -> fy
    = fun storage v -> match storage with
      | Data.Mut -> ref v
      | Data.Imm -> v

  let find_key_exn key r = find_exn key.witness r |> deref key.storage

  let find_key
    : type ty rt. < typ : ty; access : rt; .. > key -> t -> rt
    = fun key r -> match key.access with
      | Data.Exn ->
        find_key_exn key r
      | Data.Opt ->
        try Some (find_key_exn key r)
        with Not_found -> None

  let find_key_with
    : type f rt. (f, rt) Data.access -> < typ : 'ty; .. > key -> ('ty -> f) -> t -> rt
    = fun access key f r ->
      match access with
      | Data.Exn -> find_key_exn key r |> f
      | Data.Opt ->
        try Some (find_key_exn key r |> f)
        with Not_found -> None

  let add_key key v r = add key.witness (ref' key.storage v) r

  let update_key key f r =
    match find_key_exn key r with
    | x -> add_key key (f x) r
    | exception Not_found -> r

  let get : < ret : 'res; .. > get -> t -> 'res
    = fun field r -> match field with
      | Get key -> find_key key r
      | Indirect_get (key, c, access) ->
        find_key_with access key c.Bijection.to_ r

  let rec update
    : (Data.any, t) update -> t -> t
    = fun instr r -> match instr with
      | Update (key, x) -> add_key key x r
      | Fn_update (key, f) -> update_key key f r
      | Delete key -> delete_key key r
      | And (a, b) -> update b (update a r)

  let and_then l r = And (l, r)
  let ( & ) l r = and_then l r

  let set
    : type ty r. < x : ty; mut : Data.mut; ret : r > get -> ty -> t -> unit
    = fun field x r ->
      match field with
      | Get { witness; storage = Data.Mut; _ } ->
        (try find_exn witness r := x with Not_found -> ())
      | Indirect_get ({ witness; storage = Data.Mut; _ }, c, access) ->
        (try find_exn witness r := c.Bijection.of_ x with Not_found -> ())

  let rec ( .%{} )
    : type kind ret. t -> (kind * ret) field_instr -> ret
    = fun r -> function
      | Get key -> find_key key r
      | Indirect_get (key, c, access) ->
        find_key_with access key c.Bijection.to_ r
      | Update (key, x) -> add_key key x r
      | Fn_update (key, f) -> update_key key f r
      | And (a, b) -> r.%{a}.%{b}
      | Delete key -> delete_key key r

  let ( .%{}<- )
    : type ty. t -> < x : ty; mut : Data.mut; .. > get -> ty -> unit
    = fun r field x -> set field x r

  let transmute_generator
    : type ty brand.
      ('x, 'rt) Data.access ->
      < x : ty; mut : 'mut; .. > get -> (ty, 'x) Bijection.t ->
      < x : 'x; mut : 'mut; ret : 'rt > get
    = fun access instr bijection -> match instr with
      | Get witness -> Indirect_get (witness, bijection, access)
      | Indirect_get (witness, bijection', _) ->
        Indirect_get (witness, Bijection.(bijection % bijection'), access)

  let transmute field bijection = transmute_generator Data.Opt field bijection
  let ( @: ) field bijection = transmute field bijection

  let transmute_exn field bijection = transmute_generator Data.Exn field bijection
  let ( @:! ) field bijection = transmute_exn field bijection
end
