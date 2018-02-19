type value =
  [ `Null | `Bool of bool | `String of string | `Float of float ]

type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]

type t =
  [ value
  | `O of (string * t) list
  | `A of t list ]

let of_input ?(size_chunk = 0x8000) ic : t =
  let decoder = Jsonm.decoder `Manual in
  let buffer = Bytes.create size_chunk in

  let await k `Await = match input ic buffer 0 size_chunk with
    | len -> Jsonm.Manual.src decoder buffer 0 len; k ()
    | exception End_of_file -> invalid_arg "Unexpected end of input" in
  let error (`Error err) = invalid_arg (Fmt.strf "Invalid input: %a" Jsonm.pp_error err) in
  let end_of_input `End = invalid_arg "Unexpected end of input" in

  let rec arr acc k = match Jsonm.decode decoder with
    | #await as v -> await (fun () -> arr acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> value (fun v -> arr (v :: acc) k) v
  and name n k = match Jsonm.decode decoder with
    | #await as v -> await (fun () -> name n k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> value (fun v -> k (n, v)) v
  and obj acc k = match Jsonm.decode decoder with
    | #await as v -> await (fun () -> obj acc k) v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v -> invalid_arg (Fmt.strf "Unexpected lexeme: %a" Jsonm.pp_lexeme v)
  and value k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> invalid_arg "Unexpected end of object/array"
    | `Name n -> invalid_arg (Fmt.strf "Unexpected name lexeme: %s" n) in

  let rec top () = match Jsonm.decode decoder with
    | #await as v -> await top v
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme (#Jsonm.lexeme as lexeme) -> value (fun x -> x) lexeme in

  top ()
