// input.map{ case (winning, candidates) =>
//   winning.intersect(candidates).size
// }.foldLeft[Double](0) { case (points, winners) =>
//   points + (if (winners > 0) {
//     Math.pow(2, winners - 1)
//   } else {
//     0
//   }
// }.sum

input.foldLeft[(Long, Seq[Long])]((0, Seq.empty)) { case ((numExploredExtras, upcomingExtras), (winning, candidates)) =>
  val next = winning.intersect(candidates).size
  upcomingExtras.length match {
    case 0 => (numExploredExtras, (0 until next).map(_ => 1))
    case _ =>
      if (upcomingExtras.tail.length > next) {
        (numExploredExtras + upcomingExtras.head, upcomingExtras.tail.zipWithIndex.map((extra, idx) => if (idx < next) { extra + upcomingExtras.head + 1 } else { extra }))
      } else {
        (numExploredExtras + upcomingExtras.head, upcomingExtras.tail.map(extra => extra + upcomingExtras.head + 1) ++ (upcomingExtras.tail.length until next).map(_ => upcomingExtras.head + 1))
      }
  }
}

input.length
