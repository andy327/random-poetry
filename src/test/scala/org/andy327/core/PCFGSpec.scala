package org.andy327.core

import org.scalatest.FunSpec

class PCFGSpec extends FunSpec {

  describe("A PCFG Rule") {

    val pcfgStr = "S -> A B C\t\t1.23E-4"
    lazy val rule = Rule.fromPCFGString(pcfgStr)

    it("should build from a PCFG String") {
      assert(rule.lhs == "S")
      assert(rule.rhs == Seq("A", "B", "C"))
      assert(rule.prob - .000123 < 1E-10)
    }

  }

  describe("A PCFG") {

    val line = "(ROOT (S (NP (PRP I)) (VP (VBD parsed) (NP (PRP$ my) (NN test) (NN sentence)))))"
    lazy val pTree = ParseTree.fromTreeString(line)
    lazy val pcfg = new PCFG(Seq.empty[String])

    val ruleCounts = Map(
      "S -> NP VP" -> 1,
      "VBD -> parsed" -> 1,
      "NP -> PRP$ NN NN" -> 1,
      "PRP$ -> my" -> 1,
      "NN -> test" -> 1,
      "ROOT -> S" -> 1,
      "NN -> sentence" -> 1,
      "PRP -> I" -> 1,
      "VP -> VBD NP" -> 1,
      "NP -> PRP" -> 1
      )

    val binaryRuleCounts = Map(
      "S -> NP VP" -> 1,
      "VBD -> parsed" -> 1,
      "NP -> X1 NN" -> 1,
      "X1 -> PRP$ NN" -> 1,
      "PRP$ -> my" -> 1,
      "NN -> test" -> 1,
      "ROOT -> S" -> 1,
      "NN -> sentence" -> 1,
      "PRP -> I" -> 1,
      "VP -> VBD NP" -> 1,
      "NP -> PRP" -> 1
      )

    val lhsCounts = Map(
      "PRP" -> 1,
      "S" -> 1,
      "PRP$" -> 1,
      "NN" -> 2,
      "ROOT" -> 1,
      "VP" -> 1,
      "NP" -> 2,
      "VBD" -> 1
      )

    val ruleProbs = Map(
      ("PRP", "I") -> 1.0,
      ("NN", "sentence") -> 0.5,
      ("NP", "X1 NN") -> 0.5,
      ("X1", "PRP$ NN") -> 1.0,
      ("NP", "PRP") -> 0.5,
      ("VP", "VBD NP") -> 1.0,
      ("S", "NP VP") -> 1.0,
      ("ROOT", "S") -> 1.0,
      ("PRP$", "my") -> 1.0,
      ("NN", "test") -> 0.5,
      ("VBD", "parsed") -> 1.0)

    it("should correctly add rules given a set of ParseTrees") {
      pcfg.addRule(pTree)
      assert(pcfg.ruleCountMap == ruleCounts)
      assert(pcfg.lhsCountMap == lhsCounts)
    }

    it("should binarize the resulting grammar correctly") {
      pcfg.binarizeGrammar
      assert(pcfg.ruleCountMap == binaryRuleCounts)
    }

    it("should calculate the correct probabilities for given rules") {
      pcfg.calculateRuleProbabilities
      assert(pcfg.ruleProbsMap.forall{ case (k, v) => ruleProbs(k) - v < 1E-10 })
    }

  }

}
