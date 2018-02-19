module type S =
sig

  type ('a, 'b) t =
    { to_ : 'a -> 'b
    ; of_ : 'b -> 'a }

  val flip : ('a, 'b) t -> ('b, 'a) t
  val ( % ) : ('a, 'b) t -> ('c, 'a) t -> ('c, 'b) t
end

type ('a, 'b) t =
  { to_ : 'a -> 'b
  ; of_ : 'b -> 'a }

let flip x = { to_ = x.of_
             ; of_ = x.to_ }

let ( % ) { to_; of_; } s =
  { to_ = (fun x -> to_ @@ s.to_ x)
  ; of_ = (fun x -> s.of_ @@ of_ x) }
