

def isSymbol(token: Char): Boolean = !token.isDigit && token != '.'

// input.zipWithIndex.foldLeft(0L) { case (partNumSum, (gridRow, gridRowIdx)) =>
//   partNumSum + gridRow.zipWithIndex.foldLeft[(Long, Option[Either[Int, Int]])]((0, None)) { case ((gridRowSum, currentPart), (token, idx)) =>
//     currentPart match {
//       case None =>
//       	if (token.isDigit) {
//           if (
//             (idx > 0 && isSymbol(gridRow(idx - 1))) || (idx < gridRow.length() - 1 && isSymbol(gridRow(idx + 1))) || (
//               gridRowIdx > 0 && (
//                 isSymbol(input(gridRowIdx - 1)(idx)) ||
//                 (idx > 0 && isSymbol(input(gridRowIdx - 1)(idx - 1))) ||
//                 (idx < input(gridRowIdx - 1).length() - 1 && isSymbol(input(gridRowIdx - 1)(idx + 1)))
//               )
//             ) || (
//               gridRowIdx < input.length - 1 && (
//                 isSymbol(input(gridRowIdx + 1)(idx)) ||
//                 (idx > 0 && isSymbol(input(gridRowIdx + 1)(idx - 1))) ||
//                 (idx < input(gridRowIdx + 1).length() - 1 && isSymbol(input(gridRowIdx + 1)(idx + 1)))
//               )
//             )
//           ) {
//             (gridRowSum + token - '0', Some(Right(token - '0')))
//           } else {
//             (gridRowSum, Some(Left(token - '0')))
//           }
//         } else {
//           (gridRowSum, None)
//         }
//       case Some(Left(partNum)) =>
//       	if (token.isDigit) {
//           if (
//             (idx > 0 && isSymbol(gridRow(idx - 1))) || (idx < gridRow.length() - 1 && isSymbol(gridRow(idx + 1))) || (
//               gridRowIdx > 0 && (
//                 isSymbol(input(gridRowIdx - 1)(idx)) ||
//                 (idx > 0 && isSymbol(input(gridRowIdx - 1)(idx - 1))) ||
//                 (idx < input(gridRowIdx - 1).length() - 1 && isSymbol(input(gridRowIdx - 1)(idx + 1)))
//               )
//             ) || (
//               gridRowIdx < input.length - 1 && (
//                 isSymbol(input(gridRowIdx + 1)(idx)) ||
//                 (idx > 0 && isSymbol(input(gridRowIdx + 1)(idx - 1))) ||
//                 (idx < input(gridRowIdx + 1).length() - 1 && isSymbol(input(gridRowIdx + 1)(idx + 1)))
//               )
//             )
//           ) {
//             (gridRowSum + partNum * 10 + token - '0', Some(Right(partNum * 10 + token - '0')))
//           } else {
//             (gridRowSum, Some(Left(partNum * 10 + token - '0')))
//           }
//         } else {
//           (gridRowSum, None)
//         }
//       case Some(Right(partNum)) =>
//       	if (token.isDigit) {
//           (gridRowSum - partNum + partNum * 10 + token - '0', Some(Right(partNum * 10 + token - '0')))
//         } else {
//           (gridRowSum, None)
//         }
//     }
//   }._1
// }

val adjacency = Seq((-1, 0), (-1, 1), (-1, -1), (0, -1), (0, 1), (1, 0), (1, -1), (1, 1))

def stopCondition(grid: Seq[String], gridRowIdx: Int, idx: Int, visited: Set[(Int, Int)]): Boolean = visited.contains((gridRowIdx, idx)) ||
	gridRowIdx < 0 || gridRowIdx >= grid.length || idx < 0 || idx >= grid(gridRowIdx).length() || !grid(gridRowIdx)(idx).isDigit

def gearRatio(grid: Seq[String], gridRowIdx: Int, idx: Int, token: Char): Long = if (token != '*') {
  0
} else {
  adjacency.foldLeft[(Either[Option[Int], Either[(Int, Int), Unit]], Set[(Int, Int)])]((Left(None), Set.empty)) { case ((state, visited), (row, col)) =>
    state match {
      case Right(Right(_)) => (state, visited)
      case Right(Left(_)) => if (stopCondition(grid, gridRowIdx + row, idx + col, visited)) {
        (state, visited)
      } else {
        (Right(Right(())), visited)
      }
      case Left(subState) => if (stopCondition(grid, gridRowIdx + row, idx + col, visited)) {
        (state, visited)
      } else {
        val visiting = (
          (idx + col to 0 by -1).takeWhile(i => !stopCondition(grid, gridRowIdx + row, i, visited)).reverse ++
          	(idx + col + 1 until grid(gridRowIdx + row).length()).takeWhile(i => !stopCondition(grid, gridRowIdx + row, i, visited))
        ).map(i => (gridRowIdx + row, i))
        val nextPartNum = visiting.foldLeft(0) { case (partNum, (r, c)) => partNum * 10 + grid(r)(c) - '0'}
        subState match {
          case None => (Left(Some(nextPartNum)), visited ++ visiting)
          case Some(prevPartNum) => (Right(Left((prevPartNum, nextPartNum))), visited ++ visiting)
        }
      }
    }
  } match {
    case (Right(Left((a, b))), _) => a * b
    case _ => 0
  }
}

input.zipWithIndex.foldLeft(0L) { case (gearRatioSum, (gridRow, gridRowIdx)) =>
  gearRatioSum + gridRow.zipWithIndex.foldLeft(0L) { case (gridRowSum, (token, idx)) =>
    gridRowSum + gearRatio(input, gridRowIdx, idx, token)
  }
}
