let write oc iovecs =
  let open Lavoisier.IOVec in

  Lwt_list.fold_left_s
    (fun n -> function
       | { buffer = Lavoisier.Buffer.String str; off; len; } ->
         output_substring oc str off len;
         Lwt.return (n + len)
       | { buffer = Lavoisier.Buffer.Bytes str; off; len; } ->
         output_bytes oc (Bytes.sub str off len);
         Lwt.return (n + len)
       | { buffer = Lavoisier.Buffer.Bigstring bs; off; len; } ->
         for i = 0 to len - 1
         do output_char oc (Bigarray.Array1.get bs i) done;
         Lwt.return (n + len))
    0 iovecs

let comma =
  let open Farfadet in
  (fun e () -> string e ","), ()

let rec json : Condorcet.json Farfadet.t = fun e x ->
  let open Farfadet in

  let binding e (k, v) = eval e [ char $ '"'; !!string; char $ '"'; char $ ':'; !!json ] k v in
  let arr = list ~sep:comma json in
  let obj = list ~sep:comma binding in

  match x with
  | `Bool true  -> string e "true"
  | `Bool false -> string e "false"
  | `Float f    -> string e (Fmt.strf "%.16g" f)
  | `Null       -> string e "null"
  | `String s   -> eval e [ char $ '"'; !!string; char $ '"' ] s (* just for fun *)
  | `A a        -> eval e [ char $ '['; !!arr; char $ ']' ] a
  | `O o        -> eval e [ char $ '{'; !!obj; char $ '}' ] o

let with_lavoisier_a encoder payload () =
  Lwt_main.run (Condorcet.A.eval_lwt (write stderr) encoder Condorcet.jsona payload)

let with_lavoisier_b encoder payload () =
  Lwt_main.run (Condorcet.B.eval_lwt (write stderr) encoder Condorcet.jsonb payload)

let write oc iovecs =
  let open Faraday in

  List.fold_left
    (fun n -> function
       | { buffer; off; len; } ->
         for i = 0 to len - 1
         do output_char oc (Bigarray.Array1.get buffer i) done; n + len)
    0 iovecs
  |> fun n -> `Ok n

let with_faraday payload () =
  let encoder = Faraday.create 0x1000 in

  let rec writer () =
    match Faraday.serialize encoder (write stderr) with
    | `Yield -> writer ()
    | `Close -> Lwt.return () in
  let filler () =
    Farfadet.(eval encoder [ !!json ] payload);
    Lwt.return (Faraday.close encoder) in
  Lwt_main.run (Lwt.join [ writer (); filler () ])

open Core
open Core_bench.Std

let () =
  let encoder = Lavoisier.create 0x1000 in
  let payload = P.of_input Pervasives.stdin in

  let test_lavoisier_a = Bench.Test.create ~name:"lavoisier (a)" (with_lavoisier_a encoder payload) in
  let test_lavoisier_b = Bench.Test.create ~name:"lavoisier (b)" (with_lavoisier_b encoder payload) in
  let test_faraday     = Bench.Test.create ~name:"faraday" (with_faraday payload) in

  Command.run
    (Bench.make_command
       [ test_lavoisier_a
       ; test_lavoisier_b
       ; test_faraday ])
