module Impl : Meta.S with type 'a t = 'a Condorcet.A.t =
struct
  type 'a t = 'a Condorcet.A.t

  let ( <$> ) (bijection : ('a, 'b) Bijection.texn) p =
    let open Condorcet.A in
    using p bijection.Bijection.of_

  let ( <*> ) pa pb =
    let open Condorcet.A in
    pa <*> pb

  let ( <|> ) pu pv =
    let open Condorcet.A in
    pu <|> pv

  let ( *> ) pu pe =
    let open Condorcet.A in
    pu *> pe

  let ( <* ) pe pu =
    let open Condorcet.A in
    pe <* pu

  let ( <$ ) pu bijection =
    let open Condorcet.A in
    using pu bijection.Bijection.to_

  let ( $> ) pe bijection =
    let open Condorcet.A in
    using pe bijection.Bijection.of_

  let fix = Condorcet.A.fix
  let fail = Condorcet.A.fail

  let char = Condorcet.A.char
  let satisfy = Condorcet.A.satisfy
  let between = Condorcet.A.between
  let while1 = Condorcet.A.while1

  module Option =
  struct
    let (<$>) bijection p =
      let open Condorcet.A in
      using p
        (fun x -> match bijection.Bijection.of_ x with
           | Some x -> x
           | None -> Bijection.fail ())

    let ( $>)
      : unit t -> (unit, 'a) Bijection.topt -> 'a t
      = fun pe bijection ->
      let open Condorcet.A in
      using pe
        (fun x ->
           let open Bijection in
           match bijection.of_ x, bijection.kd with
           | Some x, O -> x
           | None, O -> fail ())

    let (<$ )
      : 'a t -> (unit, 'a) Bijection.topt -> unit t
      = fun pu bijection ->
      let open Condorcet.A in
      using pu
        (fun x ->
           let open Bijection in
           match bijection.to_ x, bijection.kd with
           | Some x, O -> x
           | None, O -> fail ())
  end
end
