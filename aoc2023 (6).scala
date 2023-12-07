val races = times.zip(dists)

races.map { case (time, dist) =>
  val rightSpeed = Math.floor((time + Math.sqrt(time * time - 4 * dist)) / 2)
  val leftSpeed = Math.ceil((time - Math.sqrt(time * time - 4 * dist)) / 2)
  rightSpeed - leftSpeed + 1
}.product
