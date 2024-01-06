object HelloWorld {
def main(args: Array[String]): Unit = {
//   val input2 = Seq(
// ".....",
// ".S-7.",
// ".|.|.",
// ".L-J.",
// "....."
//   )
//   val input3 = Seq(
// "..F7.",
// ".FJ|.",
// "SJ.L7",
// "|F--J",
// "LJ..."
//   )
  val input4 = Seq(
"...........",
".S-------7.",
".|F-----7|.",
".||.....||.",
".||.....||.",
".|L-7.F-J|.",
".|..|.|..|.",
".L--J.L--J.",
"..........."
  )
  val input5 = Seq(
".F----7F7F7F7F-7....",
".|F--7||||||||FJ....",
".||.FJ||||||||L7....",
"FJL7L7LJLJ||LJ.L-7..",
"L--J.L7...LJS7F-7L7.",
"....F-J..F7FJ|L7L7L7",
"....L7.F7||L7|.L7L7|",
".....|FJLJ|FJ|F7|.LJ",
"....FJL-7.||.||||...",
"....L---J.LJ.LJLJ..."
  )
  val input6 = Seq(
"FF7FSF7F7F7F7F7F---7",
"L|LJ||||||||||||F--J",
"FL-7LJLJ||||||LJL-77",
"F--JF--7||LJLJ7F7FJ-",
"L---JF-JLJ.||-FJLJJ7",
"|F|F-JF---7F7-L7L|7|",
"|FFJF7L7F-JF7|JL---7",
"7-L-JL7||F7|L7F-7F7|",
"L.L7LFJ|||||FJL7||LJ",
"L7JLJL-JLJLJL--JLJ.L"
  )
  val inputArg = input
  val s1 = inputArg.indexWhere(_.contains("S"))
  val s2 = inputArg(s1).indexOf('S')
  var sx: Option[Either[(Int, Int), Char]] = Some(Right('L'))
  println(sx)
  var bfsVisited = Set((s1, s2))
  var bfsLevel = Seq((s1 - 1, s2), (s1, s2 + 1), (s1, s2 - 1), (s1 + 1, s2)).filter { case (p1, p2) =>
    if (p1 >= 0 && p1 < inputArg.length && p2 >= 0 && p2 < inputArg(p1).length) {
      val edges: Set[(Int, Int)] = inputArg(p1)(p2) match {
        case '|' => Set((p1 - 1, p2), (p1 + 1, p2))
        case '-' => Set((p1, p2 - 1), (p1, p2 + 1))
        case 'L' => Set((p1 - 1, p2), (p1, p2 + 1))
        case 'J' => Set((p1 - 1, p2), (p1, p2 - 1))
        case '7' => Set((p1 + 1, p2), (p1, p2 - 1))
        case 'F' => Set((p1 + 1, p2), (p1, p2 + 1))
        case '.' => Set.empty
      }
      val connects = edges.contains((s1, s2))
      // if (connects) {
      //   sx = sx match {
      //     case None => Some(Left((p1 - s1, p2 - s2)))
      //     case Some(Left((q1, q2))) =>
      //       val tmp = Seq((q1, q2), (p1 - s1, p2 - s2)).sortWith { case ((a1, a2), (b1, b2)) =>
      //         if (a1 == b1) {
      //           a2 < b2
      //         } else {
      //           a1 < b1
      //         }
      //       }
      //       (tmp.head, tmp.last) match {
      //         case ((-1, 0), (0, 1)) => Some(Right('L'))
      //         case ((-1, 0), (0, -1)) => Some(Right('J'))
      //         case ((-1, 0), (1, 0)) => Some(Right('|'))
      //         case ((0, 1), (1, 0)) => Some(Right('F'))
      //         case ((0, -1), (1, 0)) => Some(Right('7'))
      //         case ((0, -1), (0, 1)) => Some(Right('-'))
      //       }
      //     case _ => sx
      //   }
      // }
      connects
    } else {
      false
    }
  }.toSet
  // println(sx)
  var depth = 0
  println(bfsLevel)
  while (bfsLevel.nonEmpty) {
    depth += 1
    val next = bfsLevel.flatMap { case (p1, p2) =>
      inputArg(p1)(p2) match {
        case '|' => Set((p1 - 1, p2), (p1 + 1, p2))
        case '-' => Set((p1, p2 - 1), (p1, p2 + 1))
        case 'L' => Set((p1 - 1, p2), (p1, p2 + 1))
        case 'J' => Set((p1 - 1, p2), (p1, p2 - 1))
        case '7' => Set((p1 + 1, p2), (p1, p2 - 1))
        case 'F' => Set((p1 + 1, p2), (p1, p2 + 1))
      }
    }.toSet.diff(bfsVisited)
    bfsVisited = bfsVisited ++ bfsLevel
    bfsLevel = next
  }
  // println(bfsVisited)
  var enclosedVisited: Set[(Int, Int)] = Set.empty
  println((0 until inputArg.length).map { p1 =>
    (0 until inputArg(p1).length).map { p2 =>
      if (!bfsVisited.contains((p1, p2)) && !enclosedVisited.contains((p1, p2))) {
        println((p1, p2))
        var islandCount = 0
        var islandExploration = Seq((p1, p2))
        while (islandExploration.nonEmpty) {
          val n1 = islandExploration.head._1
          val n2 = islandExploration.head._2
          if (!enclosedVisited.contains((n1, n2))) {
            enclosedVisited = enclosedVisited ++ Set((n1, n2))
            islandCount += 1
            islandExploration = Seq((-1, 0), (0, 1), (0, -1), (1, 0)).map { case (d1, d2) =>
              (n1 + d1, n2 + d2)
            }.filter { case (o1, o2) =>
              o1 >= 0 && o1 < inputArg.length && o2 >= 0 && o2 < inputArg(o1).length &&
              !bfsVisited.contains((o1, o2)) && !enclosedVisited.contains((o1, o2))
            } ++ islandExploration.tail
          } else {
            islandExploration = islandExploration.tail
          }
        }
        // todo (start type)
        val enclosed = Seq((-1, 0), (0, 1), (0, -1), (1, 0)).forall { case (d1, d2) =>
          var m1 = p1
          var m2 = p2
          var limit: Option[(Int, Int)] = None
          var intersections = 0
          while (m1 >= 0 && m1 < inputArg.length && m2 >= 0 && m2 < inputArg(m1).length) {
            m1 = m1 + d1
            m2 = m2 + d2
            if (bfsVisited.contains((m1, m2))) {
              val sameDirection = (if (m1 == s1 && m2 == s2) { sx.get.right.get } else { inputArg(m1)(m2) }) match {
                case '|' => d2 == 0
                case '-' => d1 == 0
                case 'L' => limit match {
                  case None =>
                    limit = if (d2 == 0) {
                      Some((0, -1))
                    } else {
                      Some((1, 0))
                    }
                    false
                  case Some((l1, l2)) =>
                    (d2 == 0 && l2 == 1) || (d1 == 0 && l1 == -1)
                }
                case 'J' => limit match {
                  case None =>
                    limit = if (d2 == 0) {
                      Some((0, 1))
                    } else {
                      Some((1, 0))
                    }
                    false
                  case Some((l1, l2)) =>
                    (d2 == 0 && l2 == -1) || (d1 == 0 && l1 == -1)
                }
                case '7' => limit match {
                  case None =>
                    limit = if (d2 == 0) {
                      Some((0, 1))
                    } else {
                      Some((-1, 0))
                    }
                    false
                  case Some((l1, l2)) =>
                    (d2 == 0 && l2 == -1) || (d1 == 0 && l1 == 1)
                }
                case 'F' => limit match {
                  case None =>
                    limit = if (d2 == 0) {
                      Some((0, -1))
                    } else {
                      Some((-1, 0))
                    }
                    false
                  case Some((l1, l2)) =>
                    (d2 == 0 && l2 == 1) || (d1 == 0 && l1 == 1)
                }
              }
              if (!sameDirection) {
                intersections += 1
              }
            }
          }
          intersections % 2 == 1
        }
        if (enclosed) {
          println(islandCount)
          islandCount
        } else {
          0
        }
      } else {
        0
      }
    }.sum
  }.sum)
}
}