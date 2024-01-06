import scala.collection.mutable

object HelloWorld {
def main(args: Array[String]): Unit = {

val example = Seq(
".|...\\....",
"|.-.\\.....",
".....|-...",
"........|.",
"..........",
".........\\",
"..../.\\\\..",
".-.-/..|..",
".|....-|.\\",
"..//.|....",
)

val inputArg = example

case class Direction(r: Int, c: Int) {
  lazy val mf = Direction(-1 * c, -1 * r)
  lazy val mb = Direction(c, r)
  def mv(p: (Int, Int)) = (p._1 + r, p._2 + c)
}
val UP = Direction(-1, 0)
val RIGHT = Direction(0, 1)
val LEFT = Direction(0, -1)
val DOWN = Direction(1, 0)

val visited: mutable.Map[(Int, Int), Set[Direction]] = mutable.Map.empty
var explore: Seq[((Int, Int), Direction)] = Seq(((0, 0), RIGHT))

while (explore.nonEmpty) {
  if (visited.contains(explore.head._1) && visited(explore.head._1).contains(explore.head._2)) {
    explore = explore.tail
  } else {
    if (!visited.contains(explore.head._1)) {
      visited(explore.head._1) = Set(explore.head._2)
    } else {
      visited(explore.head._1) = visited(explore.head._1) ++ Set(explore.head._2)
    }
    val exploreHead: Seq[((Int, Int), Direction)] = (inputArg(explore.head._1._1)(explore.head._1._2) match {
      case '/' => Seq((explore.head._2.mf.mv(explore.head._1), explore.head._2.mf))
      case '\\' => Seq((explore.head._2.mb.mv(explore.head._1), explore.head._2.mb))
      case '-' => if (explore.head._2.r == 0) {
        Seq((explore.head._2.mv(explore.head._1), explore.head._2))
      } else {
        Seq((LEFT.mv(explore.head._1), LEFT), (RIGHT.mv(explore.head._1), RIGHT))
      }
      case '|' => if (explore.head._2.c == 0) {
        Seq((explore.head._2.mv(explore.head._1), explore.head._2))
      } else {
        Seq((UP.mv(explore.head._1), UP), (DOWN.mv(explore.head._1), DOWN))
      }
      case _ => Seq((explore.head._2.mv(explore.head._1), explore.head._2))
    }).filter { case ((r, c), _) =>
      r >= 0 && r < inputArg.length && c >= 0 && c < inputArg(r).length
    }
    explore = exploreHead ++ explore.tail
  }
}

println(visited.size)

}
}