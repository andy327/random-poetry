package org.andy327.core

import scala.util.parsing.combinator._

/** Builds a tree structure from a context-free phrase structure grammar representation as generated
  * by the output of the LexicalizedParser (see http://nlp.stanford.edu/software/lex-parser.shtml)
  * from the Stanford NLP Parser.
  */
class ParseTree(val label: String,
                val terminal: Boolean,
                val children: Seq[ParseTree] = Seq.empty) {
  override def toString = (label +: children.map(_.label)).mkString("(", ",", ")")
}

object ParseTree {
  val tagSplitRegex = """(\S+) (.+)""".r

  def fromTreeString(treeString: String): ParseTree = {
    assume(treeString.startsWith("(") && treeString.endsWith(")"), "Could not parse tree from: " + treeString)
    val innerTreeString = treeString.substring(1, treeString.size - 1).replaceAll("\\s+", " ")
    val isTerminal = !innerTreeString.contains("(")
    (innerTreeString, isTerminal) match {
      case (ParseTree.tagSplitRegex(posTag, leaf), true) => { // part-of-speech node
        val children = Seq(new ParseTree(leaf, true))
        new ParseTree(posTag, false, children) // create a non-terminal node with a single, terminal child
      }
      case (ParseTree.tagSplitRegex(label, childrenString), false) => {
        val children = ChildrenParser(childrenString).get.map(ParseTree.fromTreeString(_))
        new ParseTree(label, false, children)
      }
      case _ => throw new RuntimeException("Could not parse tree from: " + treeString)
    }
  }
}

/** Extracts the children ParseTrees using a parser-combinator to parse potentially nested parenthetical
  * structures. For example, "(a) (b (c d))" is split into: "(a)", "(b (c d))"
  */
object ChildrenParser extends RegexParsers {
  lazy val childRegex = """[^\(\)]+""".r
  def paren: Parser[String] =
    "(" ~ rep1(childRegex | paren) ~ ")" ^^ {
      case open ~ child ~ close => (open :: child ::: close :: Nil) mkString ""
    }
  def apply(s: String) = parseAll(rep(paren), s)
}
