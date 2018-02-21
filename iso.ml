let () = Printexc.record_backtrace true

let to_string v =
  let encoder = Git.Minienc.create 0x100 in
  let buffer = Buffer.create 16 in

  let module B = Buffer in
  let open Git.Minienc in

  let rec go = function
    | Continue { continue; encoder; } -> go (continue encoder)
    | Flush { continue; iovecs; } ->
      List.iter
        (function
          | { IOVec.buffer = `Bigstring ba; off; len; } ->
            for i = 0 to len - 1
            do B.add_char buffer (Bigarray.Array1.get ba (off + i)) done
          | { IOVec.buffer = `String s; off; len; } ->
            B.add_substring buffer s off len
          | { IOVec.buffer = `Bytes s; off; len; } ->
            B.add_subbytes buffer s off len)
        iovecs;
      go (continue (IOVec.lengthv iovecs))
    | End v -> v in
  go (Git_unix.FS.Value.M.encoder v (flush (fun encoder -> End ())) encoder);
  B.contents buffer

module Minienc = Git.Minienc
module Git = Git_unix.FS

let safe_exn f x =
  try f x with _ -> Bijection.fail ()

let int64 =
  Bijection.make_exn
    ~fwd:(safe_exn Int64.of_string)
    ~bwd:(safe_exn Int64.to_string)

let hash =
  Bijection.make_exn
    ~fwd:(safe_exn Git.Hash.of_string)
    ~bwd:(safe_exn Git.Hash.to_string)

let perm =
  Bijection.make_exn
    ~fwd:(safe_exn Git.Value.Tree.perm_of_string)
    ~bwd:(safe_exn Git.Value.Tree.string_of_perm)

let entry =
  Bijection.make_exn
    ~fwd:(fun ((perm, name), node) -> { Git.Value.Tree.perm; name; node; })
    ~bwd:(fun { Git.Value.Tree.perm; name; node; } -> ((perm, name), node))

let tree =
  Bijection.make_exn
    ~fwd:(fun l -> Git.Value.Tree (Git.Value.Tree.of_list l))
    ~bwd:(function Git.Value.Tree l -> Git.Value.Tree.to_list l
                 | _ -> Bijection.fail ())

let char_element chr =
  Bijection.element ~compare:Char.equal ~pp:Fmt.char chr

type kind =
  [ `Tree | `Commit | `Tag | `Blob ]

let kind =
  Bijection.make_exn
    ~fwd:(function "tree" -> `Tree
                 | "commit" -> `Commit
                 | "tag" -> `Tag
                 | "blob" -> `Blob
                 | _ -> Bijection.fail ())
    ~bwd:(function `Tree -> "tree"
                 | `Commit -> "commit"
                 | `Tag -> "tag"
                 | `Blob -> "blob")

let git : (kind * Git.Value.t, Git.Value.t) Bijection.texn =
  Bijection.make_exn
    ~fwd:(function `Tree, Git.Value.Tree l -> Git.Value.Tree l
                 | #kind, _ -> Bijection.fail ())
    ~bwd:(function Git.Value.Tree _ as t -> `Tree, t
                 | _ -> Bijection.fail ())

module Make (M: Meta.S) =
struct
  open M
  open Bijection

  let is_not_sp chr = chr <> ' '
  let is_not_nl chr = chr <> '\x00'
  let is_digit = function '0' .. '9' -> true | _ -> false

  let hash = hash <$> take Git.Hash.Digest.length
  let perm = perm <$> while1 is_not_sp
  let name = while1 is_not_nl
  let kind = kind <$> (string "tree" <|> string "commit" <|> string "tag" <|> string "blob")

  let entry =
    entry <$>
    ((perm <* (char <$ char_element ' '))
     <*> (name <* (char <$ char_element '\x00'))
     <*> hash)

  let tree = tree <$> (list entry)

  let length = int64 <$> while1 is_digit

  let header =
    (kind <* (char <$ char_element ' '))
    <*> (length <* (char <$ char_element '\x00'))

  let git =
    header <*> tree
end

let pp_string = Minienc.pp_scalar ~get:String.get ~length:String.length

let main () =
  let module A = Make(Proxy_angstrom.Impl) in
  let module C = Make(Proxy_condorcet.Impl) in

  let open Lwt.Infix in
  let ( >>!= ) = Lwt_result.bind in

  Git.create () >>!= fun t ->
  Git.contents t >>!= fun l ->
  List.filter
    (function hash, Git.Value.Tree _ -> true
            | _ -> false)
    l |> fun l ->
  List.iter
    (function
      | hash, (Git.Value.Tree tree as t) ->
        let raw = to_string t in

        (match Angstrom.parse_string A.git raw with
         | Ok ((_, tree') as t') ->
           let raw' = Condorcet.to_string C.git t' in
           Fmt.(pf stdout) "%a: raw equal: %b.\n%!" Git.Hash.pp hash (String.equal raw raw');

           if not (String.equal raw raw')
           then begin
             Fmt.(pf stdout) "value source: %a.\n%!" Git.Value.pp (Git.Value.Tree tree);
             Fmt.(pf stdout) "value result: %a.\n%!" Git.Value.pp tree';

             Fmt.(pf stdout) "source: %a.\n%!" (Fmt.hvbox pp_string) raw;
             Fmt.(pf stdout) "result: %a.\n%!" (Fmt.hvbox pp_string) raw';
           end;

           Fmt.(pf stdout) "%a: hash equal: %b.\n%!" Git.Hash.pp hash Git.(Hash.equal hash (Value.digest tree'))
         | Error err ->
           Fmt.(pf stderr) "Got an error (%a): %s.\n%!"
             Git.Hash.pp (Git.Value.digest t) err)
      | _ -> assert false)
    l;
  Lwt.return (Ok ())

let () = match Lwt_main.run (main ()) with
  | Ok () -> ()
  | Error err -> Fmt.(pf stderr) "Got a git error: %a.\n%!" Git.pp_error err
