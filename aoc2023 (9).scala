object HelloWorld {
def main(args: Array[String]): Unit = {
  val input = Seq(
Seq(0,3,6,9,12,15),
Seq(1,3,6,10,15,21),
Seq(10,13,16,21,30,45)
  )
  println(input.map { history =>
    var nthHistories = Seq(history)
    while (nthHistories.last.exists(v => v != 0)) {
      nthHistories = nthHistories ++ Seq((1 until nthHistories.last.length).map(i => nthHistories.last(i) - nthHistories.last(i - 1)))
    }
    // println(nthHistories.length)
    nthHistories.foldRight(0L) { case (nthHistory, lastExtrapolation) =>
      nthHistory.head - lastExtrapolation  
    }
  }.sum)
}
}