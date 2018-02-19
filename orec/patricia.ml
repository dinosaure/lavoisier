module Endianess =
struct
  module type S =
  sig
    type mask = int

    val branching_bit: int -> int -> mask
    val mask: int -> mask -> int
    val shorter: mask -> mask -> bool
  end

  module Little: S =
  struct
    type mask = int

    let lowest_bit x = x land (-x)
    let branching_bit i0 i1 = lowest_bit (i0 lxor i1)
    let mask i m = i land (m - 1)
    let shorter : int -> int -> bool = (<)
  end

  module Big: S =
  struct
    type mask = int

    let lowest_bit x = x land (-x)
    let rec highest_bit x =
      let m = lowest_bit x in
      if x = m then m else highest_bit (x - m)
    let branching_bit i0 i1 = highest_bit (i0 lxor i1)
    let mask i m = i land (lnot (2 * m - 1))
    let shorter : int -> int -> bool = (>)
  end
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

module Make (X: Endianess.S): S =
struct
  type key = int

  type 'a t =
    | None
    | Leaf of int * 'a
    | Branch of int * X.mask * 'a t * 'a t

  exception Empty

  let empty = None

  let rec choose = function
    | None -> raise Empty
    | Leaf (key, data) -> key, data
    | Branch (_, _, t, _) -> choose t

  let rec lookup k = function
    | None -> raise Empty
    | Leaf (k', v) -> if k = k' then v else raise Not_found
    | Branch (_, m, l, r) ->
      lookup k (if k land m = 0 then l else r)

  let find = lookup

  let mem key m =
    try let _ = lookup key m in true
    with Empty | Not_found -> false

  let join p0 t0 p1 t1 =
    let m = X.branching_bit p0 p1 in
    let p = X.mask p0 (* for instance *) m in

    if p0 land m = 0
    then Branch (p, m, t0, t1)
    else Branch (p, m, t1, t0)

  let match_prefix k p m = X.mask k m = p

  type 'a decision =
    'a -> 'a -> 'a

  exception Unchanged

  let add decide k v m =
    let rec add t = match t with
      | None -> Leaf (k, v)
      | Leaf (k0, v0) ->
        if k = k0
        then let v'= decide v0 v in
          if v' == v0
          then raise Unchanged
          else Leaf (k, v')
        else join k (Leaf (k, v)) k0 t
      | Branch (p, m, tl, tr) ->
        if match_prefix k p m
        then Branch (p, m, add tl, tr)
        else Branch (p, m, tl, add tr) in
    add m

  let strict_add k v m =
    add (fun _ _ -> raise Unchanged) k v m

  let fine_add decide k v m =
    try add decide k v m
    with Unchanged -> m

  let add k v m =
    fine_add (fun _ n -> n) k v m

  let singleton k v = Leaf (k, v)

  let is_singleton = function
    | Leaf (k, v) -> Some (k, v)
    | None | Branch _ -> None

  let is_empty = function
    | None -> true
    | Leaf _ | Branch _ -> false

  let rec cardinal = function
    | None -> 0
    | Leaf _ -> 1
    | Branch (_, _, tl, tr) -> cardinal tl + cardinal tr

  let remove k m =
    let rec remove = function
      | None -> raise Empty
      | Leaf (k0, _) ->
        if k = k0
        then None
        else raise Not_found
      | Branch (p, m, tl, tr) ->
        if k land m = 0
        then match remove tl with
          | None -> tr
          | tl -> Branch (p, m, tl, tr)
        else match remove tr with
          | None -> tl
          | tr -> Branch (p, m, tl, tr) in
    try remove m
    with Not_found -> m

  let rec pop k = function
    | None -> raise Empty
    | Leaf (k0, v0) ->
      if k0 = k
      then v0, None
      else raise Not_found
    | Branch (p, m, tl, tr) ->
      if k land m = 0
      then match pop k tl with
        | data, None -> data, tr
        | data, tl -> data, Branch (p, m, tl, tr)
      else match pop k tr with
        | data, None -> data, tl
        | data, tr -> data, Branch (p, m, tl, tr)

  let reverse decision x y =
    decision y x

  let fine_union decide m0 m1 =
    let rec union s t = match s, t with
      | None, _ -> t
      | (Leaf _ | Branch _), None -> s
      | Leaf (k, v), _ ->
        fine_add (reverse decide) k v t
      | Branch _, Leaf (k, v) ->
        fine_add decide k v s
      | Branch (p, m, s0, s1),
        Branch (q, n, t0, t1) ->
        if p = q && m = n
        then
          let u0 = union s0 t0 in
          let u1 = union s1 t1 in
          if t0 == u0 && t1 == u1
          then t
          else Branch (p, m, u0, u1)
        else if X.shorter m n && match_prefix q p m
        then
          if q land m = 0
          then Branch (p, m, union s0 t, s1)
          else Branch (p, m, s0, union s1 t)
        else if X.shorter n m && match_prefix p q n
        then
          if p land n = 0
          then let u0 = union s t0 in
            if t0 == u0 then t
            else Branch (q, n, u0, t1)
          else let u1 = union s t1 in
            if t1 == u1 then t
            else Branch (q, n, t0, u1)
        else join p s q t in
    union m0 m1

  let union m0 m1 = fine_union (fun _ n -> n) m0 m1

  let rec iter f = function
    | None -> ()
    | Leaf (k, v) -> f k v
    | Branch (_, _, tl, tr) ->
      iter f tl;
      iter f tr

  let rec fold f m a =
    match m with
    | None -> a
    | Leaf (k, v) -> f k v a
    | Branch (_, _, tl, tr) ->
      fold f tl (fold f tr a)

  let fold_rev f m a =
    match m with
    | None -> a
    | Leaf (k, v) -> f k v a
    | Branch (_, _, tl, tr) ->
      fold f tr (fold f tl a)

  let rec map f = function
    | None -> None
    | Leaf (k, v) -> Leaf (k, f v)
    | Branch (p, m, tl, tr) ->
      Branch (p, m, map f tl, map f tr)

  let rec endo_map f t =
    match t with
    | None -> t
    | Leaf (k, v) ->
      let v'= f v in
      if v == v'
      then t else Leaf (k, v')
    | Branch (p, m, tl, tr) ->
      let tl' = endo_map f tl in
      let tr' = endo_map f tr in
      if tl == tl' && tr == tr'
      then t else Branch (m, p, tl', tr')

  let pp ppk ppv ppf m =
    let lst = fold (fun k v r -> (k, v) :: r) m [] in
    Fmt.(braces (list ~sep:(const string ";@ ")(pair ppk ppv))) ppf lst
end

module Little = Make(Endianess.Little)
module Big = Make(Endianess.Big)
