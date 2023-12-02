// Databricks notebook source
val input: Seq[Seq[Seq[String]]] = Seq(
  Seq(Seq("14 green", "8 blue", "9 red"), Seq("5 blue", "4 green", "2 red"), Seq("4 red", "4 blue", "4 green"), Seq("1 blue", "3 green", "2 red"), Seq("10 red", "3 blue", "15 green"), Seq("2 red", "6 green", "3 blue")),
)

// COMMAND ----------

val input2 = input.map(_.map(_.map { cubesStr =>
  val cubesSplit = cubesStr.split(" ")
  (cubesSplit(1), cubesSplit(0).toInt)
}.toMap))

// COMMAND ----------

val constraints = Map("red" -> 12, "green" -> 13, "blue" -> 14)

// COMMAND ----------

val colorIdxs = Seq("red", "green", "blue")

// COMMAND ----------

input2.foldLeft(0L){ case (minPowerSum, game) => minPowerSum + (
  game.foldLeft(Seq(0, 0, 0)) { case (mins, round) =>
    mins.zipWithIndex.map { case (prev, idx) =>
      Math.max(prev, round.getOrElse(colorIdxs(idx), 0))
    }
  }.foldLeft(1L)((power, colorCount) => power * colorCount)
)}

// COMMAND ----------


