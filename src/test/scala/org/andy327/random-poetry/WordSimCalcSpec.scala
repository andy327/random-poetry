package org.andy327.randompoetry

import scala.math._
import org.scalatest.FunSpec

class WordSimCalcSpec extends FunSpec {

  implicit def impApproxEqual(x: Double) = new { def `~=`(y: Double): Boolean = abs(x - y) < 1E-10 }
  val v1 = Map("a" -> 0.17149858514250882, "b" -> 0.34299717028501764, "c" -> 0.6859943405700353, "d" -> 0.5144957554275265, "e" -> 0.34299717028501764)
  val v2 = Map("a" -> 0.5746957711326908, "b" -> 0.19156525704423027, "f" -> 0.7662610281769211, "g" -> 0.19156525704423027, "h" -> 0.09578262852211514)
  val v3 = Map("tangent" -> 1.0)
  val v0 = Map.empty[String, Double]

  describe("An L1 Measure object") {

    it("should calculate distance between two (L2-normalized) feature vectors correctly") {
      assert(L1.distance(v1, v2) ~= 3.1517252792568153, L1.distance(v1, v2) + " != 3.1517252792568153")
    }

    it("should calculate zero distance between a (L2-normalized) feature vector and itself") {
      assert(L1.distance(v1, v1) ~= 0.0, L1.distance(v1, v1) + " != 0.0")
    }

    it("should calculate zero distance between zero vectors") {
      assert(L1.distance(v0, v0) ~= 0.0, L1.distance(v0, v0) + " != 0.0")
    }

  }

  describe("A Euclidean Measure object") {

    it("should calculate distance between two (L2-normalized) feature vectors correctly") {
      assert(Euclidean.distance(v1, v2) ~= 1.2928527737312299, Euclidean.distance(v1, v2) + " != 1.2928527737312299")
    }

    it("should calculate zero distance between a (L2-normalized) feature vector and itself") {
      assert(Euclidean.distance(v1, v1) ~= 0.0, Euclidean.distance(v1, v1) + " != 0.0")
    }

    it("should calculate zero distance between zero vectors") {
      assert(Euclidean.distance(v0, v0) ~= 0.0, Euclidean.distance(v0, v0) + " != 0.0")
    }

  }

  describe("An Cosine Measure object") {

    it("should calculate distance between two (L2-normalized) feature vectors correctly") {
      assert(Cosine.distance(v1, v2) ~= 0.8357341472722675, Cosine.distance(v1, v2) + " != 0.8357341472722675")
    }

    it("should calculate zero distance between a (L2-normalized) feature vector and itself") {
      assert(Cosine.distance(v1, v1) ~= 0.0, Cosine.distance(v1, v1) + " != 0.0")
    }

    it("should calculate zero distance between zero vectors") {
      assert(Cosine.distance(v0, v0) ~= 0.0, Cosine.distance(v0, v0) + " != 0.0")
    }

    it("should calculate 1.0 distance between (L2-normalized) orthogonal vectors") {
      assert(Cosine.distance(v1, v3) ~= 1.0, Cosine.distance(v1, v3) + " != 1.0")
    }

  }

  describe("A Jaccard Measure object") {

    it("should calculate distance between two (L2-normalized) feature vectors correctly") {
      assert(Jaccard.distance(v1, v2) ~= 0.75, Jaccard.distance(v1, v2) + " != 0.75")
    }

    it("should calculate zero distance between a (L2-normalized) feature vector and itself") {
      assert(Jaccard.distance(v1, v1) ~= 0.0, Jaccard.distance(v1, v1) + " != 0.0")
    }

    it("should calculate NaN distance between zero vectors") {
      assert(Jaccard.distance(v0, v0).isNaN, Jaccard.distance(v0, v0) + " != NaN")
    }

    it("should calculate 1.0 distance between (L2-normalized) orthogonal vectors") {
      assert(Jaccard.distance(v1, v3) ~= 1.0, Jaccard.distance(v1, v3) + " != 1.0")
    }

  }

  describe("A Dice Measure object") {

    it("should calculate distance between two (L2-normalized) feature vectors correctly") {
      assert(Dice.distance(v1, v2) ~= 0.6, Dice.distance(v1, v2) + " != 0.6")
    }

    it("should calculate zero distance between a (L2-normalized) feature vector and itself") {
      assert(Dice.distance(v1, v1) ~= 0.0, Dice.distance(v1, v1) + " != 0.0")
    }

    it("should calculate NaN distance between zero vectors") {
      assert(Dice.distance(v0, v0).isNaN, Dice.distance(v0, v0) + " != NaN")
    }

    it("should calculate 1.0 distance between (L2-normalized) orthogonal vectors") {
      assert(Dice.distance(v1, v3) ~= 1.0, Dice.distance(v1, v3) + " != 1.0")
    }

  }

  describe("A WordSimCalc") {

    it("should calculate TF vectors correctly") {
      pending
    }

    it("should calculate TF-IDF vectors correctly") {
      pending
    }

    it("should calculate PMI vectors correctly") {
      pending
    }

    it("should L2-normalize vectors correctly") {
      pending
    }

  }

}
