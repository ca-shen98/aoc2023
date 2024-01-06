import scala.collection.mutable

object HelloWorld {
def main(args: Array[String]): Unit = {

val example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

val inputArg = example.split(',')

println(inputArg.map(step => step.foldLeft(0) { case (v, c) =>
  ((v + c.toInt) * 17) % 256
}).sum)

println(inputArg.foldLeft[mutable.Map[Int, Seq[(String, Int)]]](mutable.Map.empty) { case (hm, step) =>
  step.foldLeft[(String, Int, Option[Int])](("", 0, None)) { case (s, c) => c match {
    case '-' => (s._1, s._2, s._3)
    case n if n.isDigit => (s._1, s._2, Some((n - '0').toInt))
    case '=' => s
    case l => (s._1 + l, ((s._2 + l.toInt) * 17) % 256, s._3)
  }} match {
    case (l, hb, None) =>
      if (hm.contains(hb)) {
        val idx = hm(hb).indexWhere(k => k._1 == l)
        if (idx >= 0) {
          hm(hb) = hm(hb).slice(0, idx) ++ hm(hb).slice(idx + 1, hm(hb).length)
        }
        hm
      } else {
        hm
      }
    case (l, hb, Some(f)) =>
      if (hm.contains(hb)) {
        val idx = hm(hb).indexWhere(k => k._1 == l)
        if (idx >= 0) {
          hm(hb) = hm(hb).slice(0, idx) ++ Seq((l, f)) ++ hm(hb).slice(idx + 1, hm(hb).length)
        } else {
          hm(hb) = hm(hb) ++ Seq((l, f))
        }
        hm
      } else {
        hm(hb) = Seq((l, f))
        hm
      }
  }
}.map { case (hb -> lfs) => lfs.zipWithIndex.map { case ((_, f), i) => f * (i + 1).toLong }.sum * (hb + 1) }.sum)

}
}
