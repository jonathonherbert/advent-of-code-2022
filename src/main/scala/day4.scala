package advent

object day4:
    def run() =
        val lines = readFile("resources/day4.txt")
        val intersections = lines.map { line =>
            line.split(",").toList match {
                case a :: b :: tail =>
                    val rangeA = toRange(a)
                    val rangeB = toRange(b)
                    val minRange = Math.min(rangeA.length, rangeB.length)
                    val totalIntersections = rangeA.intersect(rangeB).length >= minRange
                    val anyIntersection = rangeA.intersect(rangeB).length > 0

                    (totalIntersections, anyIntersection)
                case err => throw new Error(s"Bad match: ${err}")
            }
        }

        println(intersections.map(_._1).filter(a => a).length)
        println(intersections.map(_._2).filter(a => a).length)

    def toRange(rangeStr: String) = rangeStr.split("-").toList match {
        case a :: b :: tail => toRangeArr(a.toInt, b.toInt)
        case err => throw new Error(s"Bad match: ${err}")
    }

    def toRangeArr(a: Int, b: Int): List[Int] = (a to b).toList
