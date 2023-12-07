val times = Seq(56977793L)
val dists = Seq(499221010971440L)

// val times = Seq(7, 15, 30)
// val dists = Seq(9, 40, 200)

val races = times.zip(dists)

races.map { case (time, dist) =>
  val rightSpeed = Math.floor((time + Math.sqrt(time * time - 4 * dist)) / 2)
  val leftSpeed = Math.ceil((time - Math.sqrt(time * time - 4 * dist)) / 2)
  rightSpeed - leftSpeed + 1
}.product
