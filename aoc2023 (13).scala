object HelloWorld {
def main(args: Array[String]): Unit = {
  
val example = Seq(
Seq(
"#.##..##.",
"..#.##.#.",
"##......#",
"##......#",
"..#.##.#.",
"..##..##.",
"#.#.##.#.",
),
Seq(
"#...##..#",
"#....#..#",
"..##..###",
"#####.##.",
"#####.##.",
"..##..###",
"#....#..#",
)
)

val inputArg = example

println(inputArg.map{ pattern =>
  val rn = pattern.length
  val cn = pattern(0).length
  def checkRows(r: Int): Boolean = {
    var diff = false
    (0 until Math.min(r, rn - r)).foreach { ri =>
      (0 until cn).foreach { cj =>
        if (pattern(r - 1 - ri)(cj) != pattern(r + ri)(cj)) {
          if (diff) {
            return false
          } else {
            diff = true
          }
        }
      }
    }
    diff
  }
  def checkCols(c: Int): Boolean = {
    var diff = false
    (0 until Math.min(c, cn - c)).foreach { ci =>
      (0 until rn).foreach { rj =>
        if (pattern(rj)(c - 1 - ci) != pattern(rj)(c + ci)) {
          if (diff) {
            return false
          } else {
            diff = true
          }
        }
      }
    }
    diff
  }
  (1 until rn).find(checkRows).map(rr => 100 * rr).getOrElse((1 until cn).find(checkCols).getOrElse(0))
}.sum)

}
}