package org.andy327.core

import org.scalatest.FunSpec

class ParseTreeSpec extends FunSpec {

  val parsedSingleLine = "(ROOT (S (NP (PRP I)) (VP (VBD parsed) (NP (PRP$ my) (NN test) (NN sentence)))))"

  val parsedMultiLine = """(ROOT
  (S
    (NP (PRP I))
    (VP (VBD parsed)
      (NP (PRP$ my) (NN test) (NN sentence)))))"""

  describe("A ParseTree") {

    lazy val pTree = ParseTree.fromTreeString(parsedSingleLine)
    lazy val pTree2 = ParseTree.fromTreeString(parsedMultiLine)

    it("should build from a part-of-speech tagged phrase tree") {
      assert(pTree.label == "ROOT")
      assert(pTree.toString == "(ROOT,S)")
      assert(pTree.children.map(_.label) == Seq("S"))
      assert(pTree.children.flatMap(_.children.map(_.label)) == Seq("NP", "VP"))
      assert(pTree.children.flatMap(_.children.flatMap(_.children.map(_.label))) == Seq("PRP", "VBD", "NP"))
    }

    it("should build from the output of LexicalizedParser") {
      pTree.toString == pTree2.toString
    }

  }

}
