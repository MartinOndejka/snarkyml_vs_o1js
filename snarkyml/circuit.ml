open Snark_params.Tick.Run
open Helpers

module Add_rule = struct
  module Snark = struct
    type t = { stmt : F.t; proof : RefProof.t }
  end

  module Init = struct
    type _ Snarky_backendless.Request.t +=
      | A : F.t Snarky_backendless.Request.t
      | B : F.t Snarky_backendless.Request.t

    let handler (a : F.t) (b : F.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | A ->
          respond (Provide a)
      | B ->
          respond (Provide b)
      | _ ->
          respond Unhandled

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let a = exists ~request:(fun () -> A) F.typ in
      let b = exists ~request:(fun () -> B) F.typ in
      Pickles.Inductive_rule.
        { previous_proof_statements = []
        ; public_output = Field.(a + b)
        ; auxiliary_output = ()
        }

    let rule : _ Pickles.Inductive_rule.t =
      { identifier = "init"
      ; prevs = []
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end

  module Merge = struct
    type _ Snarky_backendless.Request.t +=
      | Statement1 : F.t Snarky_backendless.Request.t
      | Proof1 : RefProof.t Snarky_backendless.Request.t
      | Statement2 : F.t Snarky_backendless.Request.t
      | Proof2 : RefProof.t Snarky_backendless.Request.t

    let handler (s1 : Snark.t) (s2 : Snark.t)
        (Snarky_backendless.Request.With { request; respond }) =
      match request with
      | Statement1 ->
          respond (Provide s1.stmt)
      | Proof1 ->
          respond (Provide s1.proof)
      | Statement2 ->
          respond (Provide s2.stmt)
      | Proof2 ->
          respond (Provide s2.proof)
      | _ ->
          respond Unhandled

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let stmt1 = exists ~request:(fun () -> Statement1) F.typ in
      let proof1 = exists ~request:(fun () -> Proof1) RefProof.typ in

      let stmt2 = exists ~request:(fun () -> Statement2) F.typ in
      let proof2 = exists ~request:(fun () -> Proof2) RefProof.typ in

      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = stmt1
              ; proof_must_verify = Boolean.true_
              ; proof = proof1
              }
            ; { public_input = stmt2
              ; proof_must_verify = Boolean.true_
              ; proof = proof2
              }
            ]
        ; public_output = Field.(stmt1 + stmt2)
        ; auxiliary_output = ()
        }

    let rule self : _ Pickles.Inductive_rule.t =
      { identifier = "merge"
      ; prevs = [ self; self ]
      ; main
      ; feature_flags = Pickles_types.Plonk_types.Features.none_bool
      }
  end
end
