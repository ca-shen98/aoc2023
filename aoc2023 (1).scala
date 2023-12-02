// Databricks notebook source
// https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm
case class DigitParserTrieNode(node: Either[Map[Char, DigitParserTrieNode], Int])
val fwdTrieParser = DigitParserTrieNode(Left(Map(
  'o' -> DigitParserTrieNode(Left(Map(
    'n' -> DigitParserTrieNode(Left(Map(
      'e' -> DigitParserTrieNode(Right(1))
    )))
  ))),
  't' -> DigitParserTrieNode(Left(Map(
    'w' -> DigitParserTrieNode(Left(Map(
      'o' -> DigitParserTrieNode(Right(2))
    ))),
    'h' -> DigitParserTrieNode(Left(Map(
      'r' -> DigitParserTrieNode(Left(Map(
        'e' -> DigitParserTrieNode(Left(Map(
          'e' -> DigitParserTrieNode(Right(3))
        )))
      )))
    )))
  ))),
  'f' -> DigitParserTrieNode(Left(Map(
    'o' -> DigitParserTrieNode(Left(Map(
      'u' -> DigitParserTrieNode(Left(Map(
        'r' -> DigitParserTrieNode(Right(4))
      )))
    ))),
    'i' -> DigitParserTrieNode(Left(Map(
      'v' -> DigitParserTrieNode(Left(Map(
        'e' -> DigitParserTrieNode(Right(5))
      )))
    )))
  ))),
  's' -> DigitParserTrieNode(Left(Map(
    'i' -> DigitParserTrieNode(Left(Map(
      'x' -> DigitParserTrieNode(Right(6))
    ))),
    'e' -> DigitParserTrieNode(Left(Map(
      'v' -> DigitParserTrieNode(Left(Map(
        'e' -> DigitParserTrieNode(Left(Map(
          'n' -> DigitParserTrieNode(Right(7))
        )))
      )))
    )))
  ))),
  'e' -> DigitParserTrieNode(Left(Map(
    'i' -> DigitParserTrieNode(Left(Map(
      'g' -> DigitParserTrieNode(Left(Map(
        'h' -> DigitParserTrieNode(Left(Map(
          't' -> DigitParserTrieNode(Right(8))
        )))
      )))
    )))
  ))),
  'n' -> DigitParserTrieNode(Left(Map(
    'i' -> DigitParserTrieNode(Left(Map(
      'n' -> DigitParserTrieNode(Left(Map(
        'e' -> DigitParserTrieNode(Right(9))
      )))
    )))
  )))
)))
val bwdTrieParser = DigitParserTrieNode(Left(Map(
  'e' -> DigitParserTrieNode(Left(Map(
    'n' -> DigitParserTrieNode(Left(Map(
      'o' -> DigitParserTrieNode(Right(1)),
      'i' -> DigitParserTrieNode(Left(Map(
        'n' -> DigitParserTrieNode(Right(9))
      )))
    ))),
    'e' -> DigitParserTrieNode(Left(Map(
      'r' -> DigitParserTrieNode(Left(Map(
        'h' -> DigitParserTrieNode(Left(Map(
          't' -> DigitParserTrieNode(Right(3))
        )))
      )))
    ))),
    'v' -> DigitParserTrieNode(Left(Map(
      'i' -> DigitParserTrieNode(Left(Map(
        'f' -> DigitParserTrieNode(Right(5))
      )))
    )))
  ))),
  'o' -> DigitParserTrieNode(Left(Map(
    'w' -> DigitParserTrieNode(Left(Map(
      't' -> DigitParserTrieNode(Right(2))
    )))
  ))),
  'r' -> DigitParserTrieNode(Left(Map(
    'u' -> DigitParserTrieNode(Left(Map(
      'o' -> DigitParserTrieNode(Left(Map(
        'f' -> DigitParserTrieNode(Right(4))
      )))
    )))
  ))),
  'x' -> DigitParserTrieNode(Left(Map(
    'i' -> DigitParserTrieNode(Left(Map(
      's' -> DigitParserTrieNode(Right(6))
    )))
  ))),
  'n' -> DigitParserTrieNode(Left(Map(
    'e' -> DigitParserTrieNode(Left(Map(
      'v' -> DigitParserTrieNode(Left(Map(
        'e' -> DigitParserTrieNode(Left(Map(
          's' -> DigitParserTrieNode(Right(7))
        )))
      )))
    )))
  ))),
  't' -> DigitParserTrieNode(Left(Map(
    'h' -> DigitParserTrieNode(Left(Map(
      'g' -> DigitParserTrieNode(Left(Map(
        'i' -> DigitParserTrieNode(Left(Map(
          'e' -> DigitParserTrieNode(Right(8))
        )))
      )))
    )))
  )))
)))

// COMMAND ----------

def foldIterate(initial: Map[Char, DigitParserTrieNode])(active: Either[Seq[Map[Char, DigitParserTrieNode]], Int], token: Char): Either[Seq[Map[Char, DigitParserTrieNode]], Int] = active match {
  case Right(_) => active
  case Left(parsers) => if (token.isDigit) {
    Right(token - '0')
  } else {
    val next = parsers.flatMap(_.get(token))
    next.find(parser => parser match {
      case DigitParserTrieNode(Right(_)) => true
      case _ => false
    }).map(parser => Right(parser.node.right.get)).getOrElse(Left(next.map(_.node.left.get) ++ Seq(initial)))
  }
}

// COMMAND ----------

input.foldLeft(0L) { (calibrationSum: Long, calibrationString: String) =>
  val firstDigit = calibrationString.foldLeft[Either[Seq[Map[Char, DigitParserTrieNode]], Int]](Left(Seq(fwdTrieParser.node.left.get)))(foldIterate(fwdTrieParser.node.left.get))
  val lastDigit = calibrationString.foldRight[Either[Seq[Map[Char, DigitParserTrieNode]], Int]](Left(Seq(bwdTrieParser.node.left.get)))((token, parser) => foldIterate(bwdTrieParser.node.left.get)(parser, token))
  calibrationSum + firstDigit.right.get * 10 + lastDigit.right.get
}

// COMMAND ----------


