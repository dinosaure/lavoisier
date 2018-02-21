type ('f, 't, 'is) kind =
  | Exn : ('a, 'a, exn) kind
  | Opt : ('a, 'a option, opt) kind
  | Res : ('a, ('a, error) result, res) kind
and exn = E
and opt = O
and res = R
and error = [ `Msg of string ]

let exn = Exn
let opt = Opt
let res = Res

type ('k, 'a, 'b) t =
  { to_ : 'a -> 'rb
  ; of_ : 'b -> 'ra
  ; kd  : 'kd }
  constraint 'k =
    < reta : ('a, 'ra, 'kd) kind
  ; retb : ('b, 'rb, 'kd) kind >

type ('a, 'b) texn =
  (< reta : ('a, 'a, exn) kind
   ; retb : ('b, 'b, exn) kind >, 'a, 'b) t

type ('a, 'b) topt =
  (< reta : ('a, 'a option, opt) kind
   ; retb : ('b, 'b option, opt) kind >, 'a, 'b) t

type ('a, 'b) tres =
  (< reta : ('a, ('a, error) result, res) kind
   ; retb : ('b, ('b, error) result, res) kind >, 'a, 'b) t

let make
  : type a b ra rb kd.
    (a, ra, kd) kind -> (b, rb, kd) kind
    -> fwd:(a -> rb)
    -> bwd:(b -> ra)
    -> (< reta : (a, ra, kd) kind
        ; retb : (b, rb, kd) kind >, a, b) t
  = fun k k' ~fwd ~bwd ->
          { to_ = fwd
          ; of_ = bwd
          ; kd  = (match k, k' with
  | Exn, Exn -> E
  | Opt, Opt -> O
  | Res, Res -> R) }

let make_exn ~fwd ~bwd = make exn exn ~fwd ~bwd
let make_opt ~fwd ~bwd = make opt opt ~fwd ~bwd
let make_res ~fwd ~bwd = make res res ~fwd ~bwd

let flip
  : (< reta : ('a, 'ra, 'kd) kind
     ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
    (< reta : ('b, 'rb, 'kd) kind
     ; retb : ('a, 'ra, 'kd) kind >, 'b, 'a) t
  = fun x -> { to_ = x.of_
             ; of_ = x.to_
             ; kd  = x.kd }

let compose
  : (< reta : ('a, 'ra, 'kd) kind
     ; retb : ('b, 'rb, 'kd) kind >, 'a, 'b) t ->
    (< reta : ('b, 'rb, 'kd) kind
     ; retb : ('c, 'rc, 'kd) kind >, 'b, 'c) t ->
    (< reta : ('a, 'ra, 'kd) kind
     ; retb : ('c, 'rc, 'kd) kind >, 'a, 'c) t
  = fun { to_; of_; kd; } s ->
  { to_ = (fun x -> s.to_ @@ to_ x)
  ; of_ = (fun x -> of_ @@ s.of_ x)
  ; kd  = kd }

let ( % ) = compose

let commute =
  { to_ = (fun (a, b) -> (b, a))
  ; of_ = (fun (b, a) -> (a, b))
  ; kd  = E }

external identity : 'a -> 'a = "%identity"

let identity =
  { to_ = identity
  ; of_ = identity
  ; kd  = E }

exception Bijection

let fail () = raise Bijection

let element ~compare ~pp x =
  { to_ = (fun () -> x)
  ; of_ =
      (fun x' ->
         if compare x x'
         then ()
         else raise Bijection)
  ; kd  = E }
