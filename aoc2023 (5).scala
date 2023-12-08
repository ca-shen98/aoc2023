
seeds.map { case (seedStart, seedRange) =>
  maps.foldLeft(Seq((seedStart, seedStart + seedRange)) { case (idRangesToMap, inputMapper) =>
    idRangesToMap.flatMap { case (idRangeStart, idRangeEnd) =>
      val maybeIdRangeStartIdx = inputMapper.indexWhere(_.sourceStart > idRangeStart) - 1
      val maybeIdRangeEndIdx = inputMapper.indexWhere(_.sourceStart >= idRangeEnd) - 1
      val idRangeStartIdx = if (maybeIdRangeStartIdx < -1) inputMapper.length else maybeIdRangeStartIdx
      val idRangeEndIdx = if (maybeIdRangeEndIdx < -1) inputMapper.length else maybeIdRangeEndIdx
      if (idRangeEndIdx == idRangeStartIdx) {
        if (idRangeStartIdx < 0 || idRangeStartIdx >= inputMapper.length || inputMapper(idRangeStartIdx).sourceEnd <= idRangeStart) {
          (idRangeStart, idRangeEnd)
        } else if (inputMapper(idRangeEndIdx).sourceEnd <= idRangeEnd) {
          (
            inputMapper(idRangeStartIdx).destStart + idRangeStart - inputMapper(idRangeStartIdx).sourceStart,
            inputMapper(idRangeStartIdx).destStart + idRangeEnd - inputMapper(idRangeStartIdx).sourceStart
          )
        } else {
          Seq(
            (
            	inputMapper(idRangeStartIdx).destStart + idRangeStart - inputMapper(idRangeStartIdx).sourceStart,
              inputMapper(idRangeStartIdx).destEnd
            ), (
            	inputMapper(idRangeStartIdx).sourceEnd,
              idRangeEnd
            )
          )
        }
      } else {
        (
          if (idRangeStartIdx < 0 || inputMapper(idRangeStartIdx).sourceEnd <= idRangeStart) {
            Seq((idRangeStart, inputMapper(idRangeStartIdx + 1).sourceStart))
          } else {
            Seq(
              (
                inputMapper(idRangeStartIdx).destStart + idRangeStart - inputMapper(idRangeStartIdx).sourceStart,
                inputMapper(idRangeStartIdx).destEnd
              ), (
                inputMapper(idRangeStartIdx).sourceEnd,
                inputMapper(idRangeStartIdx + 1).sourceStart
              )
            )
          }
        ) ++ (1 until idRangeEndIdx - idRangeStartIdx - 1).map { idx =>
          Seq(
            (inputMapper(idRangeStartIdx + idx).destStart, inputMapper(idRangeStartIdx + idx).destEnd),
            (inputMapper(idRangeStartIdx + idx).sourceEnd, inputMapper(idRangeStartIdx + idx + 1).sourceStart)
          )
        } ++ (
          if (idxRangeEndIdx >= inputMapper.length) {
            Seq(
              (inputMapper(idRangeEndIdx - 1).sourceEnd, idxRangeEnd),
              (inputMapper(idRangeEndIdx - 1).sourceEnd, idxRangeEnd)
            )
          } else {
            Seq(
              
            )
          }
        )
      }
    }.sorted.foldLeft[Seq[(Long, Long)]](Seq.empty) { case (consolidated, next) =>
      
    }
	}.first._1
}.min
