object HelloWorld {
def main(args: Array[String]): Unit = {

val input = Seq(
"...........#..........................#..................................................................#......#......#....................",
"...#....................#..........................................#..............#...........#.............................................",
".............................................................................................................................#..............",
"........#...........#.............#..........#.....#...............................................................#........................",
".............................#..............................................................................................................",
"........................................................................................................#..................................#",
".....#..........#....................#.................#............................#.......................................................",
".....................................................................#...................#.......#....................................#.....",
"............................................................................................................................................",
"........................................#....................................................................#..............#...............",
"...........#........................................#............#.....................................................#...........#........",
"......................#.............#..........................................................#.................#..........................",
".............................................#..........................................................#...................................",
".#.............................#............................#...............#.............#...................................#.............",
"............................................................................................................................................",
"...................................................................................#........................................................",
".......#.......................................#...............#..........................................#.........#................#......",
"....................#..............#....................................................#........#........................................#.",
"...................................................#...................#.....................................................#..............",
"..........................#....................................................#.............................#..............................",
"............................................................................................................................................",
".........#.....#...........................#...............................................#.......................#........................",
"..........................................................................................................................#.............#...",
".............................................................#..............#.........#...........................................#.........",
"..................................#....................................................................#....................................",
".................................................................................................................#..........................",
".............#......................................................#.............................#..................................#......",
"......................#.....#...........#........#.........#.....................#.........................#...............#................",
"................................................................#................................................................#..........",
".....#.......................................#.......................................................................#......................",
".........................................................................#................#..........#......................................",
"..................#.....#.....................................................#..............................#..............................",
".....................................#..................................................................................................#...",
".............................................................#..........................................#......................#............",
".....................................................#..............#................#............#...................#.....................",
"............................................................................#........................................................#......",
"...#.......#................................................................................................................................",
".................#............................#..............................................................#............#.................",
"...........................#.............................................................#......#...........................................",
"...............................................................#........#...........#.......................................................",
"......#......................................................................................................................#......#......#",
"........................................................#................................................#..................................",
"...............................#................#.............................#.............#...............................................",
".........................................#...........................#......................................................................",
".#.........................#........#...........................#..................#..................#.....................................",
".....................................................#........................................................#...........#.................",
".......#..........#......................................................................#..........................#.............#.......#.",
"............................................................................................................................................",
"............................................................................................................................................",
"..................................................................................................#.........................................",
"..................................#.............................#......#..............#..............................................#......",
"............................................................................................................................................",
".............................................#...................................#........................#.................................",
".....#......#............#.....................................................................................#..................#.........",
"...........................................................#.................................................................#..............",
"................................#...................................................................................#.......................",
"................................................#..............................................#........#...................................",
"..........#..........#.....#........................................................#...........................................#...........",
"......................................#........................................#..........#..............................................#..",
"...................................................................#.....#.............................................#....................",
".....#..........#.........................................#.................................................................................",
".............................................#....................................#...........#.............................................",
"..........................................................................................................#......................#.........#",
".#.................#............#........#..................................................................................................",
"...........................#..............................................................#..................................#..............",
".............#......................#.................................................................#.................................#...",
".......................#............................#......#....................................#...........................................",
"........#......................................#....................#.......................................................................",
"...............................#...............................................................................#............................",
"................................................................#.................#........#................................................",
".#.....................................................................................................................#....................",
"................#..........#.......................#....................................................#...................#........#......",
"..............................................................................................#.............................................",
"..........#............#.............................................#......................................................................",
"...............................#.....#....................#.................................................................................",
".....................................................................................#.............................................#........",
"..........................................................................................#........................#........................",
"...#....................................#..........................#........................................................................",
"............................#.......................................................................#.......................................",
"..............................................#.............................................................................................",
"..................#.....#.......................................#...........#.........................................................#.....",
"#...........#.......................................................................................................#.......................",
"..........................................#............#.........................#......................#...................................",
".............................................................#............................#.................................................",
".........#..........#...............................................#...........................#.......................#..................#",
"..........................#......................#....................................#......................................#..............",
".....................................#.............................................................................................#........",
".............#..................#...........................................................................................................",
".......#..................................#..............................#.................#.....................#......................#...",
".....................#......................................#...............................................................................",
".............................#.........................#..........................#..............#..........................................",
".......................................#................................................................#...........#.......................",
"............................................................................................................................................",
"............................................................................................................................................",
"..................#............#............#.......#.......................#...............................................#...............",
".#........#...........................................................#...............................................#.....................",
"...............................................................................................................#............................",
"...................................................................................#.......................................................#",
"................................................#...........#...............................................................................",
"...................................................................................................#..............#.........................",
".........#............#..............#.................................#...................#...................................#............",
"................#..............#...................#.............................#..........................................................",
".............................................#................#........................................#..................#.................",
".....#.........................................................................................................#...................#........",
"...................................#......................#........#......................................................................#.",
"........................#.............................................................#........#............................................",
"..#......................................#....................................#.............................................................",
"................#...............#.....................#..........................................................#..........................",
".....................................................................................................#.......................#..............",
"...............................................#...........................................................................................#",
".........#...........#.....................................#................#........................................#......................",
"...#..........#.........................#............................#...............................................................#......",
"............................................................................................................#.............#.................",
"..........................#...................................#............................#...................................#............",
".....................................................................................#......................................................",
"..........#..............................................................................................................................#..",
"................#..............#.............#.........................#................................#...........................#.......",
"....#...............................#.......................#....................#..........................................................",
"....................................................#...........................................................#............#..............",
".................................................................................................#..........................................",
"............................................................................................................................................",
".........#...........#.....................#................................#..........#...........................#........................",
"...................................#........................................................................................................",
"..................................................................................#................................................#........",
"..............#.........................................................#.................................#...................#.........#...",
"..#...........................................#.............................................................................................",
"...........................#.........................................................................................#......................",
"...............................................................#............................................................................",
".........#..........#....................................................................#.......#......#........................#..........",
"...................................#.................#...........................#.............................#............................",
".....#.............................................................#........................................................................",
".............................#..............................................................................................................",
"..............#.........................#...........................................................#..........................#.........#..",
"................................................................#...........................................................................",
"..#.........................................#.................................#..................................#..........................",
".......................#.......#.......................................#....................#..............................#................",
"....................................................................................#...................#..........................#........",
".....#.....#....................................#...................................................................#.......................",
"..........................................#............#....................................................................................",
"...............#............................................#..................................#............................................"
)
  
val example = Seq(
"...#......",
".......#..",
"#.........",
"..........",
"......#...",
".#........",
".........#",
"..........",
".......#..",
"#...#.....",
)

val inputArg = input
val inputColLength = inputArg(0).length

val expandedRows = (0 until inputArg.length).filter(r => !inputArg(r).contains("#"))
val expandedCols = (0 until inputColLength).filter(c => inputArg.forall(r => r(c) == '.'))

val galaxies = (0 until inputArg.length).flatMap(r => (0 until inputArg(r).length).flatMap(c =>
  if (inputArg(r)(c) == '#') {
    Some((r, c))
  } else {
    None
  }
))

println((0 until galaxies.length).flatMap(i => (i + 1 until galaxies.length).map { j =>
  (galaxies(j)._1 - galaxies(i)._1).abs + (galaxies(j)._2 - galaxies(i)._2).abs + 999999L * (
    expandedRows.filter(r => r > Math.min(galaxies(i)._1, galaxies(j)._1) && r < Math.max(galaxies(i)._1, galaxies(j)._1)).length +
    expandedCols.filter(c => c > Math.min(galaxies(i)._2, galaxies(j)._2) && c < Math.max(galaxies(i)._2, galaxies(j)._2)).length
  )
}).sum)

}
}