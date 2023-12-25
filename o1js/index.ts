import { Field, SelfProof, ZkProgram } from "o1js";

const AddRules = ZkProgram({
  name: "add",
  publicOutput: Field,

  methods: {
    init: {
      privateInputs: [Field, Field],

      method(a, b) {
        return a.add(b);
      },
    },

    merge: {
      privateInputs: [SelfProof, SelfProof],

      method(snark1: SelfProof<void, Field>, snark2: SelfProof<void, Field>) {
        snark1.verify();
        snark2.verify();

        return snark1.publicOutput.add(snark2.publicOutput);
      },
    },
  },
});

let start: number;

const main = async () => {
  console.log("compiling");
  start = Date.now();
  await AddRules.compile();
  console.log("compile", Date.now() - start);

  start = Date.now();
  const proof1 = await AddRules.init(Field(4), Field(5));
  console.log("proof1", Date.now() - start);
  console.log(proof1.publicOutput.toBigInt());
  proof1.verify();

  start = Date.now();
  const proof2 = await AddRules.init(Field(1), Field(2));
  console.log("proof2", Date.now() - start);
  console.log(proof2.publicOutput.toBigInt());
  proof2.verify();

  start = Date.now();
  const proof = await AddRules.merge(proof1, proof2);
  console.log("proof", Date.now() - start);
  console.log(proof.publicOutput.toBigInt());
  proof.verify();
};

main().catch(console.error);
