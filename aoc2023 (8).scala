val init = mapper.keys.filter(key => key.endsWith("A")).toSeq
init.foreach { key =>
var steps = 0
var curr: Either[Seq[String], Int] = Left(Seq(key))
while (curr.isLeft) {
  val iter = sequence.zipWithIndex.foldLeft[Either[Seq[String], Int]](curr) { case (locs, (dir, idx)) => locs match {
    case Right(_) => locs
    case Left(strs) =>
      val next = strs.map { str =>
        val instr = mapper(str)
        if (dir == 'R') {
          instr._2
        } else {
          instr._1
        }
      }
      // println(next)
      if (next.forall(_.endsWith("Z"))) {
        Right(idx + 1)
      } else {
        Left(next)
      }
  } }
  if (iter.isLeft) {
    steps += sequence.length
    curr = iter
  } else {
    steps += iter.right.get
    curr = Right(steps)
  }
}
println(curr)
}
