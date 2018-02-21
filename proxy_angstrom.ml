module Impl : Meta.S with type 'a t = 'a Angstrom.t =
struct
  type 'a t = 'a Angstrom.t

  let ( <$> ) bijection p =
    let open Angstrom in
    try bijection.Bijection.to_ <$> p
    with Bijection.Bijection -> fail "bijection"

  let ( <*> ) pa pb =
    let open Angstrom in
    pa >>= fun a -> pb >>= fun b -> return (a, b)

  let ( <|> ) pu pv =
    let open Angstrom in
    pu <|> pv

  let ( *> ) pu pe =
    let open Angstrom in
    pu *> pe

  let ( <* ) pe pu =
    let open Angstrom in
    pe <* pu

  let ( $> )
    : unit t -> (unit, 'a) Bijection.texn -> 'a t
    = fun pu bijection ->
      let open Angstrom in
      pu >>= fun () ->
      let open Bijection in
      match bijection.to_ (), bijection.kd with
      | x, E -> return x
      | exception Bijection -> Angstrom.fail "bijection"

  let ( <$ )
    : 'a t -> (unit, 'a) Bijection.texn -> unit t
    = fun pe bijection ->
      let open Angstrom in
      pe >>= fun x ->
      let open Bijection in
      match bijection.of_ x, bijection.kd with
      | x, E -> return x
      | exception Bijection -> Angstrom.fail "bijection"

  let fix = Angstrom.fix
  let fail = Angstrom.fail "error"

  let char = Angstrom.any_char
  let satisfy = Angstrom.satisfy
  let between p s a = p *> a <* s
  let while1 = Angstrom.take_while1
  let take = Angstrom.take
  let list ?sep p = match sep with
    | Some sep -> Angstrom.sep_by sep p
    | None -> Angstrom.many p
  let string = Angstrom.string

  module Option =
  struct
    let ( <$> ) bijection p =
      let open Angstrom in
      p >>= fun x -> match bijection.Bijection.to_ x with
      | Some x -> return x
      | None -> fail "bijection"

    let ( $>) pu bijection =
      let open Angstrom in
      pu >>= fun () ->
      let open Bijection in
      match bijection.to_ (), bijection.kd with
      | Some x, O -> return x
      | None, O -> Angstrom.fail "bijection"

    let (<$ ) pe bijection =
      let open Angstrom in
      pe >>= fun x ->
      let open Bijection in
      match bijection.of_ x, bijection.kd with
      | Some x, O -> return x
      | None, O -> Angstrom.fail "bijection"
  end
end
