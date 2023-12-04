// input.map{ case (winning, candidates) =>
//   winning.intersect(candidates).size
// }.zipWithIndex.foldLeft(input.map(_ => 1L)) { case (copies, (winners, idx)) =>
//   println(copies)
//   copies.slice(0, idx + 1) ++ copies.slice(idx + 1, idx + winners).map(a => a + copies(idx)) ++ copies.slice(idx + winners, copies.length)
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
