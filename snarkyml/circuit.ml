open Snark_params.Tick.Run
open Helpers

module Add_rule = struct
  module Statement = struct
    type t = { result : F.t } [@@deriving snarky]
  end

  module Snark = struct
    type t = { stmt : Statement.t; proof : RefProof.t } [@@deriving snarky]
  end

  include Snark

  module Init = struct
    module Witness = struct
      type t = { a : F.t; b : F.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let Witness.{ a; b } = exists_witness () in
      Pickles.Inductive_rule.
        { previous_proof_statements = []
        ; public_output = Statement.{ result = Field.(a + b) }
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
    module Witness = struct
      type t = { s1 : Snark.t; s2 : Snark.t } [@@deriving snarky]
    end

    include MkHandler (Witness)

    let%snarkydef_ main Pickles.Inductive_rule.{ public_input = () } =
      let Witness.{ s1; s2 } = exists_witness () in
      Pickles.Inductive_rule.
        { previous_proof_statements =
            [ { public_input = s1.stmt
              ; proof_must_verify = Boolean.true_
              ; proof = s1.proof
              }
            ; { public_input = s2.stmt
              ; proof_must_verify = Boolean.true_
              ; proof = s2.proof
              }
            ]
        ; public_output =
            Statement.{ result = Field.(s1.stmt.result + s2.stmt.result) }
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
