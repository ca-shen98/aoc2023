import scala.collection.mutable

object HelloWorld {
def main(args: Array[String]): Unit = {
  
val input = Seq(
('R',6,"#70c710"),
('D',5,"#0dc571"),
('L',2,"#5713f0"),
('D',2,"#d2c081"),
('R',2,"#59c680"),
('D',2,"#411b91"),
('L',5,"#8ceee2"),
('U',2,"#caa173"),
('L',1,"#1b58a2"),
('U',2,"#caa171"),
('R',2,"#7807d2"),
('U',3,"#a77fa3"),
('L',2,"#015232"),
('U',2,"#7a21e3"),
)

case class Direction(r: Int, c: Int) {
  
}
val UP = Direction(-1, 0)
val RIGHT = Direction(0, 1)
val LEFT = Direction(0, -1)
val DOWN = Direction(1, 0)
def dl(dl: Char): Direction = dl match {
  case 'U' => UP
  case 'R' => RIGHT
  case 'L' => LEFT
  case 'D' => DOWN
}

val trench: mutable.Map[(Int, Int), Seq[(Direction, String)]] = mutable.Map((0, 0) -> Seq((dl(input.head._1), input.head._3)))
val (f, trn, tcn) = input.map(di => (dl(di._1), di._2, di._3)).foldLeft((0, 0), 1, 1) { case (o, (d, n, hc)) =>
  (1 to n).foldLeft(o) { case ((p, trn, tcn), _) =>
    val q = (p._1 + d.r, p._2 + d.c)
    if (trench.contains(q)) {
      trench(q) = trench(q) ++ Seq((d, hc))
    } else {
      trench(q) = Seq((d, hc))
    }
    (q, Math.max(q._1 + 1, trn), Math.max(q._2, tcn))
  }
}
println(trench.keys.count(k => trench(k).length > 0))
println(trench.keys.count(k => trench(k).length > 1))
println(trench.keys.count(k => trench(k).length > 2))

// val UBIAS = Some(-1)
// val DBIAS = Some(1)

// val inner: mutable.Set[(Int, Int)] = mutable.Set.empty
// val outer: mutable.Set[(Int, Int)] = mutable.Set.empty
// (0 until trn).foreach { r => (0 until trc).foreach { c =>
//   if (!trench.contains((r, c)) && !inner.contains((r, c)) && !outer.contains((r, c))) {
//     val (en, _) = (c until tcn).foldLeft[(Int, Option[Int])]((0, None)) { case (o, c) =>
//       trench.get((r, c)) match {
//         case None => o
//         case edges =>
//           val (n, bias) = o
//           edges.length match {
//             case 1 => bias match {
//               case None => (n + 1, bias)
//               case _ => (n, bias)
//             }
//             case 2 => bias match {
//               case None => (n, Some(edges.find { case (d, _) => d.c == 0 }.r * -1))
//               case Some(br) => if (edges.find { case (d, _) => d.c == 0 }.r == br) {
//                 (n, None)
//               } else {
//                 (n + 1, None)
//               }
//             }
//           }
//       }
//     }
    
//   }
// }}

}
}