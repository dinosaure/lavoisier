let random_cstruct ~length =
  let time = Unix.gettimeofday () in
  let fingerprint = Cstruct.create 8 in
  Cstruct.BE.set_uint64 fingerprint 0 Int64.(of_float (time *. 1000.));
  Nocrypto.Rng.reseed fingerprint;
  Nocrypto.Rng.generate length

module MQueue = Queue
module AQueue = AQueue.Make(struct type t = int64 let sentinel = 0L end)

let int64_array_payload ~length =
  random_cstruct ~length:(8 * length)

let test_fqueue_push payload queue =
  let len = Cstruct.len payload / 8 in
  let res = ref queue in

  fun () ->
    for i = 0 to len - 1
    do res := FQueue.push !res (Cstruct.BE.get_uint64 payload (i * 8)) done

let test_squeue_push payload queue =
  let len = Cstruct.len payload / 8 in

  fun () ->
    for i = 0 to len - 1
    do MQueue.push (Cstruct.BE.get_uint64 payload (i * 8)) queue done

let test_aqueue_push payload queue =
  let len = Cstruct.len payload / 8 in

  fun () ->
    for i = 0 to len - 1
    do AQueue.enqueue (Cstruct.BE.get_uint64 payload (i * 8)) queue done

let test_lqueue_push payload queue =
  let len = Cstruct.len payload / 8 in
  let res = ref queue in

  fun () ->
    for i = 0 to len - 1
    do res := LQueue.push !res (Cstruct.BE.get_uint64 payload (i * 8)) done

open Core
open Core_bench.Std

let test_cqueue_push payload queue =
  let len = Cstruct.len payload / 8 in

  fun () ->
    for i = 0 to len - 1
    do Queue.enqueue queue (Cstruct.BE.get_uint64 payload (i * 8)) done

let () =
  let payload = int64_array_payload ~length:1000 in

  Command.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Okazaki's queue (push)" (test_fqueue_push payload FQueue.empty)
       ; Bench.Test.create ~name:"List's queue (push)"    (test_lqueue_push payload LQueue.empty)
       ; Bench.Test.create ~name:"Stdlib's queue (push)"  (test_squeue_push payload (MQueue.create ()))
       ; Bench.Test.create ~name:"Core's queue (push)"    (test_cqueue_push payload (Queue.create ()))
       ; Bench.Test.create ~name:"Faraday's queue (push)" (test_aqueue_push payload (AQueue.create 1000)) ])
