let () = Printexc.record_backtrace true

type t =
  | Int : int -> t
  | Add : t * t -> t
  | Sub : t * t -> t

let rec eval = function
  | Int n -> n
  | Add (a, b) -> (eval a) + (eval b)
  | Sub (a, b) -> (eval a) - (eval b)

let int : (int, t) Bijection.texn =
  Bijection.make_exn
    ~fwd:(fun n -> Int n)
    ~bwd:(function (Int n) -> n
                 | _ -> Bijection.fail ())

let add =
  Bijection.make_exn
    ~fwd:(fun (e1, e2) -> Add (e1, e2))
    ~bwd:(function Add (e1, e2) -> (e1, e2)
                 | _ -> Bijection.fail ())

let sub =
  Bijection.make_exn
    ~fwd:(fun (e1, e2) -> Sub (e1, e2))
    ~bwd:(function Sub (e1, e2) -> (e1, e2)
                 | _ -> Bijection.fail ())

let number : (string, int) Bijection.texn =
  Bijection.make_exn
    ~fwd:int_of_string
    ~bwd:string_of_int

let char_element chr =
  Bijection.element ~compare:Char.equal ~pp:Fmt.char chr

module Make (M: Meta.S) =
struct
  open M
  open Bijection

  let lp = char <$ char_element '('
  let rp = char <$ char_element ')'

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false

  let exp = fix @@ fun exp ->
    (add <$> between lp rp (exp <*> ((char <$ char_element '+') *> exp))) <|>
    (sub <$> between lp rp (exp <*> ((char <$ char_element '-') *> exp))) <|>
    ((compose number int) <$> while1 is_digit)
end

let () =
  let module A = Make(Ang.Impl) in
  let module C = Make(Con.Impl) in
  match Angstrom.parse_string A.exp "(1+(3-2))" with
  | Ok v -> Fmt.(pf stdout) "iso: %s = %d.\n%!" (Condorcet.A.to_string C.exp v) (eval v)
  | Error _ -> Fmt.(pf stderr) "Retrieve an error.\n%!"
