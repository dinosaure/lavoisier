type vec =
  { off : int option
  ; len : int option }

type 'a state = 'a Lavoisier.state
type encoder = Lavoisier.encoder
type bigstring = Lavoisier.bigstring

type 'a t =
  { run : 'r. (encoder -> 'r state) -> encoder -> 'a -> 'r state }
type 'a s =
  { sub : 'r. (encoder -> 'r state) -> encoder -> ?off:int -> ?len:int -> 'a -> 'r state }

let char    : char  t = { run = fun k e v -> Lavoisier.write_char v k e }
let int8    : int   t = { run = fun k e v -> Lavoisier.write_uint8 v k e }
let beint16 : int   t = { run = fun k e v -> Lavoisier.BE.write_uint16 v k e }
let beint32 : int32 t = { run = fun k e v -> Lavoisier.BE.write_uint32 v k e }
let beint64 : int64 t = { run = fun k e v -> Lavoisier.BE.write_uint64 v k e }
let leint16 : int   t = { run = fun k e v -> Lavoisier.LE.write_uint16 v k e }
let leint32 : int32 t = { run = fun k e v -> Lavoisier.LE.write_uint32 v k e }
let leint64 : int64 t = { run = fun k e v -> Lavoisier.LE.write_uint64 v k e }

let bool : bool t = { run = fun k e -> function
  | true  -> char.run k e '1'
  | false -> char.run k e '0' }

let substring    : string    s = { sub = fun k e ?off ?len v -> Lavoisier.write_string ?off ?len v k e }
let subbytes     : bytes     s = { sub = fun k e ?off ?len v -> Lavoisier.write_bytes ?off ?len v k e }
let subbigstring : bigstring s = { sub = fun k e ?off ?len v -> Lavoisier.write_bigstring ?off ?len v k e }

let blitter length blit : _ s = { sub = fun k e ?off ?len v ->
  Lavoisier.write k ~blit ~length ?off ?len v e }

let whole (a : 'v s) : 'v t = { run = fun k e v -> a.sub ?off:None ?len:None k e v }
let sub (a : 'v s) : (vec * 'v) t = { run = fun k e ({ off; len; }, v) -> a.sub ?off ?len k e v }

let string    : string    t = whole substring
let bytes     : bytes     t = whole subbytes
let bigstring : bigstring t = whole subbigstring

let list ?sep a : 'a list t =
  let sep k e = match sep with
    | None -> k e
    | Some a -> a.run k e () in
  let rec run k e : _ list -> _ state = function
    | [] -> k e
    | [ x ] ->
      a.run k e x
    | x :: r ->
      a.run (sep (fun e -> run k e r)) e x in
  { run }

let nop = { run = fun k e _ -> k e }

let option f : 'a option t =
  { run = fun k e -> function
    | Some v -> f.run k e v
    | None -> k e }

let compose f g = { run = fun k e (x, y) -> f.run (fun e -> g.run k e y) e x }

let using a f =
  { run = fun k e v -> a.run k e (f v) }

let map a f =
  { run = fun k e v -> (f a).run k e v }

let const a v =
  { run = fun k e () -> a.run k e v }

let ( >>= ) = map
let ( >>| ) = using
let ( <*> ) = compose
let ( <!> ) = const

let prefix p r =
  { run = fun k e v -> p.run (fun e -> r.run k e v) e () }
let suffix s r =
  { run = fun k e v -> r.run (fun e -> s.run k e ()) e v }

let ( <+> ) r s = suffix s r

let run : 'a t -> (encoder -> 'r state) -> encoder -> 'a -> 'r state = fun p k e v -> p.run k e v

let l_brack = string <!> "["
let r_brack = string <!> "]"
let l_paren = string <!> "("
let r_paren = string <!> ")"
let l_brace = string <!> "{"
let r_brace = string <!> "}"

let from : ('a -> 'u t) -> 'a t = fun f -> { run = fun k e v -> (f v).run k e v }

let newline = int8 <!> 0x0a
let flush a = { run = fun k e v -> a.run (fun e -> Lavoisier.flush k e) e v }

let bracks a = a >>= prefix l_brack >>= suffix r_brack
let parens a = a >>= prefix l_paren >>= suffix r_paren
let braces a = a >>= prefix l_brace >>= suffix r_brace

let dquote = char <!> '"'
let comma = char <!> ','

type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of json list
  | `O of (string * json) list ]

let keval
  : 'v 'r. (encoder -> 'r state) -> (Lavoisier.IOVec.t list -> int) -> encoder -> 'v t -> 'v -> 'r
  = fun k w e t v ->
  let rec go = function
    | Lavoisier.End v -> v
    | Lavoisier.Continue { continue; encoder; } ->
      continue encoder |> go
    | Lavoisier.Flush { continue; iovecs; } ->
      let len = w iovecs in
      continue len |> go in
  t.run k e v |> go

let eval w e t v = keval (fun e -> Lavoisier.End ()) w e (flush t) v

let keval_lwt
  : 'v 'r. (encoder -> 'r state) -> (Lavoisier.IOVec.t list -> int Lwt.t) -> encoder -> 'v t -> 'v -> 'r Lwt.t
  = fun k w e t v ->
  let open Lwt.Infix in

  let rec go = function
    | Lavoisier.End v -> Lwt.return v
    | Lavoisier.Continue { continue; encoder; } ->
      continue encoder |> go
    | Lavoisier.Flush { continue; iovecs; } ->
      w iovecs >|= continue >>= go in
  t.run k e v |> go

let eval_lwt w e t v = keval_lwt (fun e -> Lavoisier.End ()) w e (flush t) v

module A =
struct
  type endian =
    | Little
    | Big

  type 'a t =
    | Const  : 'a t * 'a -> unit t
    | Int16  : endian -> int t
    | Int32  : endian -> int32 t
    | Int64  : endian -> int64 t
    | Conv   : ('a -> 'b) * ('b -> 'a) * 'b t -> 'a t
    | Nop    : unit t
    | Bol    : bool t
    | Chr    : char t
    | Sub    : vec * 'a atom -> 'a t
    | Str    : string t
    | Opt    : 'a t -> 'a option t
    | Seq    : 'a t * 'b t -> ('a * 'b) t
    | Arr    : unit t * 'a t -> 'a array t
    | Lst    : unit t * 'a t -> 'a list t
    | Mu     : ('a t -> 'a t) -> 'a t
    | Union  : 't case list -> 't t
    | Prefix : unit t * 'a t -> 'a t
    | Suffix : 'a t * unit t -> 'a t
  and 't case =
    | Case : 'a t * ('t -> 'a option) * ('a -> 't) -> 't case
  and 'a atom =
    | String    : string atom
    | Bytes     : bytes atom
    | Bigstring : Lavoisier.bigstring atom
  and vec = { off : int option
            ; len : int option }

  let _bool k e = function
    | true  -> Lavoisier.write_uint8 1 k e
    | false -> Lavoisier.write_uint8 0 k e

  let _char k e c = Lavoisier.write_char c k e

  let _leint16, _leint32, _leint64 =
    (fun k e v -> Lavoisier.LE.write_uint16 v k e),
    (fun k e v -> Lavoisier.LE.write_uint32 v k e),
    (fun k e v -> Lavoisier.LE.write_uint64 v k e)

  let _beint16, _beint32, _beint64 =
    (fun k e v -> Lavoisier.BE.write_uint16 v k e),
    (fun k e v -> Lavoisier.BE.write_uint32 v k e),
    (fun k e v -> Lavoisier.BE.write_uint64 v k e)

  let _string k e s = Lavoisier.write_string s k e

  let rec encode
    : type a. (encoder -> 'r state) -> encoder -> a t -> a -> 'r state
    = fun k e -> function
      | Const (t, a)        -> (fun () -> encode k e t a)
      | Int16 Little        -> _leint16 k e
      | Int32 Little        -> _leint32 k e
      | Int64 Little        -> _leint64 k e
      | Int16 Big           -> _beint16 k e
      | Int32 Big           -> _beint32 k e
      | Int64 Big           -> _beint64 k e
      | Nop                 -> (fun () -> k e)
      | Bol                 -> _bool k e
      | Chr                 -> _char k e
      | Str                 -> _string k e
      | Sub ({ off; len; }, String)    -> (fun s -> Lavoisier.write_string ?off ?len s k e)
      | Sub ({ off; len; }, Bytes)     -> (fun s -> Lavoisier.write_bytes ?off ?len s k e)
      | Sub ({ off; len; }, Bigstring) -> (fun s -> Lavoisier.write_bigstring ?off ?len s k e)
      | Opt t               -> (function Some v -> encode k e t v | None -> k e)
      | Arr (sep, t)        -> _array (sep, t) k e
      | Lst (sep, t)        -> _list (sep, t) k e
      | Conv (to_, of_, t)  -> (fun v -> encode k e t (to_ v))
      | Mu self             -> encode k e (self (Mu self))
      | Union l ->
        (fun v ->
           let rec do_cases = function
             | [] -> invalid_arg "encode: invalid case list"
             | Case (t, to_, _) :: r ->
               match to_ v with
               | Some v -> encode k e t v
               | None -> do_cases r in
           do_cases l)
      | Seq (ta, tb) ->
        (fun (a, b) ->
           let k e = encode k e tb b in
           encode k e ta a)
      | Prefix (tp, ta) ->
        (fun a -> let k e = encode k e ta a in encode k e tp ())
      | Suffix (ta, tp) ->
        (fun a -> let k e = encode k e tp () in encode k e ta a)
  and _list
    : type a. (unit t * a t) -> (encoder -> 'r state) -> encoder -> a list -> 'r state
    = fun (sep, t) k e l ->
      let rec go l e = match l with
        | [] -> k e
        | [ x ] -> encode k e t x
        | x :: r ->
          let k e = encode (go r) e sep () in
          encode k e t x in
      go l e
  and _array
    : type a. (unit t * a t) -> (encoder -> 'r state) -> encoder -> a array -> 'r state
    = fun (sep, t) k e a -> _list (sep, t) k e (Array.to_list a)

  let (<&>) a b = Seq (a, b)
  let ( *>) p a = Prefix (p, a)
  let (<* ) a p = Suffix (a, p)

  let string  = Str
  let char    = Chr
  let bool    = Bol
  let leint16 = (Int16 Little)
  let leint32 = (Int32 Little)
  let leint64 = (Int64 Little)
  let beint16 = (Int16 Big)
  let beint32 = (Int32 Big)
  let beint64 = (Int64 Big)

  let none = Nop
  let const a x = Const (a, x)
  let list ?(sep = none) a = Lst (sep, a)
  let array ?(sep = none) a = Arr (sep, a)
  let conv fof fto t = Conv (fof, fto, t)
  let case t fto fof = Case (t, fto, fof)
  let union = function
    | [] -> invalid_arg "union: empty list"
    | cases -> Union cases
  let mu f = Mu f
  let sub ?off ?len x = Sub ({ off; len; }, x)
  let whole x = sub x
  let string = whole String
  let bytes = whole Bytes
  let bigstring = whole Bigstring
  let const_sub t ?off ?len x = Const (Sub ({ off; len; }, t), x)

  let ($) = const

  module Const =
  struct
    let char x = Const (Chr, x)
    let string ?off ?len x = const_sub String ?off ?len x
    let bytes ?off ?len x = const_sub Bytes ?off ?len x
    let bigstring ?off ?len x = const_sub Bigstring ?off ?len x
  end

  type ('f, 'r) fmt =
    | Nil: ('r, 'r) fmt
    | Snoc: ('f, 'a -> 'r) fmt * ('a -> 'r) -> ('f, 'r) fmt

  let rec write writer = function
    | Lavoisier.Flush { continue; iovecs; } ->
      let len = writer iovecs in
      write writer (continue len)
    | Lavoisier.Continue { continue; encoder; } ->
      write writer (continue encoder)
    | Lavoisier.End v -> v

  let eval : type a r. (encoder -> r) -> encoder -> a t -> a -> r
    = fun k e t a ->
      write (fun _ -> assert false) (encode (fun e -> Lavoisier.End (k e))  e t a)

  let compose f g = fun x -> f (g x)
  let (<.>) = compose

  let null_case =
    case
      (Const.string "null")
      (function `Null -> Some ()
              | _ -> None)
      (function () -> `Null)
  let bool_case =
    case
      bool
      (function `Bool v -> Some v
              | _ -> None)
      (fun v -> `Bool v)
  let float_case =
    let float v = `Float v in
    case
      string
      (function `Float f -> Some (Fmt.strf "%.16g" f)
              | _ -> None)
      (float <.> float_of_string)
  let string_case =
    case
      string
      (function `String s -> Some s | _ -> None)
      (fun s -> `String s)

  let l_brack = Const.char '['
  let r_brack = Const.char ']'
  let l_brace = Const.char '{'
  let r_brace = Const.char '}'
  let comma   = Const.char ','
  let colon   = Const.char ':'

  let json : json t =
    mu @@ fun json ->
    let arr_case : json list t = l_brack *> (list ~sep:comma json) <* r_brack in
    let binding : (string * json) t =
      conv
        (fun (k, v) -> ((k, ()), v))
        (fun ((k, ()), v) -> (k, v))
        (string <&> colon <&> json) in
    let obj_case : (string * json) list t = l_brace *> (list ~sep:comma binding) <* r_brace in
    union [ null_case
          ; bool_case
          ; float_case
          ; string_case
          ; case arr_case
              (function `A l -> Some l | _ -> None)
              (fun l -> `A l)
          ; case obj_case
              (function `O l -> Some l | _ -> None)
              (fun l -> `O l) ]
end

module B =
struct
  type ('a, 'r) t = (encoder -> 'r) -> encoder -> 'a -> 'r

  type ('ty, 'v) order =
    | Const : 'a * ('a, 'v) t -> ('v, 'v) order
    | Atom  : ('a, 'v) t -> ('a -> 'v, 'v) order

  type ('ty, 'v) fmt =
    | [] : ('v, 'v) fmt
    | (::) : ('ty, 'v) order * ('v, 'r) fmt -> ('ty, 'r) fmt

  let keval_order
    : type ty v.
      encoder
      -> (ty, v) order
      -> (encoder -> v)
      -> ty
    = fun encoder order k -> match order with
      | Const (v, t) ->
        t k encoder v
      | Atom t ->
        fun v -> t k encoder v

  let rec keval
    : type ty v.
      encoder
      -> (ty, v state) fmt
      -> (encoder -> v state)
      -> ty
    = fun encoder l k -> match l with
      | [] -> k encoder
      | order :: r ->
        let k encoder = keval encoder r k in
        keval_order encoder order k
end
