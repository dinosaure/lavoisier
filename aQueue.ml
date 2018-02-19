[@@@landmark "auto"]

module Make (T: sig type t val pp : t Fmt.t val sentinel : t end): sig
  type elem = T.t

  type t

  exception Empty

  val create : int -> t

  val is_empty : t -> bool

  val set_last : (elem -> elem option) -> t -> unit

  val push : elem -> t -> unit
  val cons : elem -> t -> unit
  val shift : t -> elem

  val map_to_list : t -> f:(elem -> 'b) -> 'b list
  val to_list : t -> elem list

  val fold : ('a -> elem -> 'a) -> 'a -> t -> 'a

  val pp: t Fmt.t
end = struct
  type elem = T.t

  type t =
    { mutable elements : elem array
    ; mutable front    : int
    ; mutable back     : int
    ; mutable size     : int }

  exception Empty

  let sentinel = T.sentinel

  let create size =
    { elements = Array.make size sentinel; front = 0; back = 0; size }

  external compare_int : int -> int -> int = "caml_int_compare" [@@noalloc]

  let is_empty t =
    compare_int t.front t.back = 0

  let ensure_space t =
    if compare_int t.back (t.size - 1) = 0
    then begin
      let len = t.back - t.front in
      if t.front > 0 then begin
        (* Shift everything to the front of the array and then clear out
         * dangling pointers to elements from their previous locations. *)
        Array.blit t.elements t.front t.elements 0 len;
        Array.fill t.elements len t.front sentinel
      end else begin
        let old  = t.elements in
        let new_ = Array.make (2 * t.size) sentinel in
        Array.blit old t.front new_ 0 len;
        t.elements <- new_;
        t.size <- 2 * t.size
      end;
      t.front <- 0;
      t.back <- len
    end

  let push e t =
    ensure_space t;
    t.elements.(t.back) <- e;
    t.back <- t.back + 1

  let set_last f t =
    match f t.elements.(t.back - 1) with
    | Some v -> t.elements.(t.back - 1) <- v
    | None -> ()

  let shift t =
    if not (is_empty t)
    then
      let result = Array.unsafe_get t.elements t.front in
      t.front <- t.front + 1;
      result
    else raise Empty

  let cons e t =
    (* This is in general not true for Deque data structures, but the usage
     * below ensures that there is always space to push an element back on the
     * front. A [enqueue_front] is always preceded by a [dequeue], with no
     * intervening operations. *)
    assert (t.front > 0);
    t.front <- t.front - 1;
    t.elements.(t.front) <- e

  let map_to_list t ~f =
    let result = ref [] in
    for i = t.back - 1 downto t.front do
      result := f t.elements.(i) :: !result
    done;
    !result

  external identity : 'a -> 'a = "%identity"

  let to_list = map_to_list ~f:identity

  let fold f a t =
    let a = ref a in

    for i = t.back - 1 downto t.front do
      a := f !a t.elements.(i)
    done;

    !a

  let pp ppf { elements } =
    Fmt.brackets (Fmt.array ~sep:(Fmt.const Fmt.char ';') T.pp) ppf elements
end
