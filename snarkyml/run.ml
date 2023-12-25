open Async
open Async_kernel
open Core_kernel
open Snark_params.Tick.Run
open Helpers
module Nat = Pickles_types.Nat
include Circuit.Add_rule

let constraint_constants = Genesis_constants.Constraint_constants.compiled

let _, _, _, Pickles.Provers.[ init_; merge_ ] =
  time "compile" (fun () ->
      Pickles.compile () ~override_wrap_domain:Pickles_base.Proofs_verified.N1
        ~cache:Cache_dir.cache ~public_input:(Output Statement.typ)
        ~auxiliary_typ:Typ.unit
        ~branches:(module Nat.N2)
        ~max_proofs_verified:(module Nat.N2)
        ~name:"add rules"
        ~constraint_constants:
          (Genesis_constants.Constraint_constants.to_snark_keys_header
             constraint_constants )
        ~choices:(fun ~self -> [ Init.rule; Merge.rule self ]) )

let init a b =
  let%map stmt, _, proof = init_ ~handler:(Init.handler { a; b }) () in
  ({ stmt; proof } : t)

let merge (s1 : t) (s2 : t) =
  let%map stmt, _, proof = merge_ ~handler:(Merge.handler { s1; s2 }) () in
  ({ stmt; proof } : t)

let () =
  Thread_safe.block_on_async_exn (fun () ->
      let%bind first =
        dtime "first" (init Field.Constant.(of_int 4) Field.Constant.(of_int 5))
      in

      let%bind second =
        dtime "second"
          (init Field.Constant.(of_int 1) Field.Constant.(of_int 2))
      in

      let%bind sum = dtime "sum" (merge first second) in

      print_endline @@ Field.Constant.to_string sum.stmt.result ;

      Deferred.unit )
