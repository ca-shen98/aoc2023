// Databricks notebook source
def handRank(hand: String): Int = {
  val jokerCount = hand.count(card => card == 'J')
  val cardCounts = hand.filter(card => card != 'J').groupBy(identity).mapValues(_.size)
  (cardCounts.headOption.map(_ => cardCounts.values.product).getOrElse(0) + jokerCount) match {
    case 5 if (cardCounts.size == 2) => 2 // XXJYY
    case 5 => 0 // JJJJJ, XXXXX, JJJJX, XXXXJ, JJJXX, XXXJJ
    case 4 if (cardCounts.size == 3) => 4 // XXYYZ
    case 4 => 1 // XXXXY, JJJXY, XXXJY, JJXXY
    case 6 => 2 // XXXYY
    case 3 => 3 // XXXYZ, JJXYZ, XXJYZ
    case 2 => 5 // XXYZW, JXYZW
    case 1 => 6
  }
}

// COMMAND ----------

def cardRank(card: Char): Int = card match {
  case 'A' => 0
  case 'K' => 1
  case 'Q' => 2
  case 'J' => 12
  case 'T' => 3
  case _ => 10 - (card - '2') + 1
}

// COMMAND ----------

def tiebreak(hand1: String, hand2: String): Boolean = {
  val (card1, card2) = hand1.zip(hand2).find { case (card1, card2) => card1 != card2 }.get
  cardRank(card1) > cardRank(card2)
}

// COMMAND ----------

val sorted = input.sortWith { case ((hand1, _), (hand2, _)) =>
  val type1 = handRank(hand1)
  val type2 = handRank(hand2)
  type1 > type2 || (type1 == type2 && tiebreak(hand1, hand2))
}

// COMMAND ----------

sorted.zipWithIndex.map { case ((_, bid), idx) => bid * (idx + 1)}.sum

// COMMAND ----------


