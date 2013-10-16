package org.andy327.randompoetry

import scala.math._

object `package` {
  type FeatureVector = Map[String, Double]
  type ContextVector = Map[String, Double]
  type Sentence = Seq[String]
}

sealed abstract class Weighting
case object Tf extends Weighting
case object Tfidf extends Weighting
case object Pmi extends Weighting

abstract class Measure {
  /* The feature vectors are assumed to be L2-normalized at this point */
  def distance(v1: FeatureVector, v2: FeatureVector): Double
}

case object L1 extends Measure {
  def distance(v1: FeatureVector, v2: FeatureVector): Double = {
    (v1.keySet union v2.keySet).toSeq.map{ k => abs(v1.getOrElse(k, 0.0) - v2.getOrElse(k, 0.0)) }.sum
  }
}

case object Euclidean extends Measure {
  def distance(v1: FeatureVector, v2: FeatureVector): Double = {
    sqrt((v1.keySet union v2.keySet).toSeq.map{ k => pow(v1.getOrElse(k, 0.0) - v2.getOrElse(k, 0.0), 2) }.sum)
  }
}

case object Cosine extends Measure {
  def distance(v1: FeatureVector, v2: FeatureVector): Double = {
    if (v1.isEmpty || v2.isEmpty) 0.0
    else 1 - (v1.keySet intersect v2.keySet).toSeq.map{ k => v1(k) * v2(k) }.sum
  }
}

case object Jaccard extends Measure {
  def distance(v1: FeatureVector, v2: FeatureVector): Double = {
    val intersection = (v1.keySet intersect v2.keySet).size
    val union = (v1.keySet union v2.keySet).size
    (union - intersection).toDouble / union
  }
}

case object Dice extends Measure {
  def distance(v1: FeatureVector, v2: FeatureVector): Double = {
    val intersection = (v1.keySet intersect v2.keySet).size
    val union = (v1.keySet union v2.keySet).size
    1 - 2.0 * intersection / (union + intersection)
  }
}

abstract class SentencePreprocessor(stopWords: Seq[String]) {
  def preprocess(line: String): Seq[String]
}

case class SimpleSentencePreprocessor(stopWords: Seq[String] = Seq.empty[String]) extends SentencePreprocessor(stopWords) {
  def preprocess(line: String): Seq[String] = {
    line.toLowerCase.split(" ").filterNot(w => stopWords.contains(w))
  }
}

class WordSimCalc(
  contextSize: Int = 2,
  matchThreshold: Int = 3,
  weighting: Weighting = Pmi,
  measure: Measure = Cosine,
  sentencePreprocessor: SentencePreprocessor = new SimpleSentencePreprocessor) {

  /** count of all occurences of all words in total */
  var totalWordCount = 0L

  /** count of all sentences added in total */
  var totalSentenceCount = 0L

  /** count of occurences of given word in all sentences */
  val wordCounts = collection.mutable.Map.empty[String, Int].withDefaultValue(0)

  /** count of sentences containing a given word */
  val wordSentenceCounts = collection.mutable.Map.empty[String, Int].withDefaultValue(0)

  /** Counts of each word's appearance within the context of a given word */
  val wordContextCounts = collection.mutable.Map.empty[String, collection.mutable.Map[String, Int]]

  def weightedVector(word: String): FeatureVector = weighting match {
    case Tf => tfVector(word: String)
    case Tfidf => tfidfVector(word: String)
    case Pmi => pmiVector(word: String)
  }

  def tfVector(word: String): FeatureVector = {
    l2Normalize(wordContextCounts(word).mapValues(_.toDouble).toMap)
  }

  def tfidfVector(word: String): FeatureVector = {
    def idf(w: String) = log10(totalSentenceCount.toDouble / wordSentenceCounts(w))
    l2Normalize(wordContextCounts(word).map{ case (w, count) => w -> count * idf(w) }.toMap)
  }

  def pmiVector(word: String): FeatureVector = {
    val p_w = wordCounts(word).toDouble / totalWordCount
    def p_f(feature: String) = wordCounts(feature).toDouble / totalWordCount
    def p_wf(feature: String) = wordContextCounts(word).getOrElse(feature, 0).toDouble / totalWordCount
    def pmi(feature: String) = log10(p_wf(feature) / (p_w * p_f(feature)))
    l2Normalize(wordContextCounts(word).map{ case (w, count) => w -> pmi(w) }.toMap)
  }

  def l2Normalize(v: FeatureVector): FeatureVector = {
    val l2Length = sqrt(v.values.map(pow(_, 2)).sum)
    v.mapValues(_ / l2Length)
  }

  def addLine(line: String) = {
    val wordList = sentencePreprocessor.preprocess(line)

    totalWordCount += wordList.size

    totalSentenceCount += 1

    for (w <- wordList) wordCounts(w) += 1

    for (w <- wordList.distinct) wordSentenceCounts(w) += 1

    val newContexts = wordList.zipWithIndex
      .map{ case (w, i) => w -> (i - contextSize to i + contextSize).filterNot(n => n < 0 || n >= wordList.size || n == i).map(wordList(_)) }
    for ((w, context) <- newContexts) {
      val counts = wordContextCounts.getOrElse(w, collection.mutable.Map()).withDefaultValue(0)
      for(neighbor <- context) counts(neighbor) += 1
      wordContextCounts.update(w, counts)
    }
  }

  def sentenceVector(sentence: Sentence): ContextVector = {
    null // TODO: Implement
  }

  def mostSimilarSentence(sentence: Sentence, choices: Seq[Sentence]): Sentence = {
    require(!choices.isEmpty)
    null // TODO: Implement
  }
}
