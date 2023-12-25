open Mina_base
open Snark_params.Tick.Run
open Async_kernel

module F = struct
  type t = field

  type var = Field.t

  let typ : (var, t) Typ.t = Field.typ
end

module MkRef (T : sig
  type t
end) =
struct
  type t = T.t

  type var = T.t As_prover.Ref.t

  let typ : (var, t) Typ.t = Typ.Internal.ref ()
end

module RefProof = MkRef (Proof)

module MkHandler (Witness : sig
  type t

  type var

  val typ : (var, t) Typ.t
end) =
struct
  open Snarky_backendless.Request

  type _ t += Witness : Witness.t t

  let handler (w : Witness.t) (With { request; respond }) =
    match request with Witness -> respond (Provide w) | _ -> respond Unhandled

  let exists_witness () : Witness.var =
    exists Witness.typ ~request:(fun () -> Witness)
end

let time lab f =
  let open Core_kernel in
  let start = Time.now () in
  let x = f () in
  let stop = Time.now () in
  printf "%s: %s\n%!" lab (Time.Span.to_string_hum (Time.diff stop start)) ;
  x

let dtime label (d : 'a Deferred.t) =
  let open Core_kernel in
  let start = Time.now () in
  let%bind x = d in
  let stop = Time.now () in
  printf "%s: %s\n%!" label (Time.Span.to_string_hum @@ Time.diff stop start) ;
  return x
