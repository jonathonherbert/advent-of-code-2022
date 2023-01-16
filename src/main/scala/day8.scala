package advent

case class Tree(height: Int, visible: Boolean = false)

object day8:
  def run() =
    val input = readFile("resources/day8.txt")
    val grid = input.map(_.map(x => Tree(x.toString.toInt)).toList)

    def getIndexesOfVisiblePositions(
        remainingChars: List[Int],
        index: Int = 0,
        lastHighestHeight: Int = 0,
        visibleIndexes: List[Int] = List(0)
    ): List[Int] = remainingChars match {
      case head :: tail =>
        if (head > lastHighestHeight)
          getIndexesOfVisiblePositions(
            tail,
            index + 1,
            head,
            visibleIndexes :+ index
          )
        else
          getIndexesOfVisiblePositions(
            tail,
            index + 1,
            lastHighestHeight,
            visibleIndexes
          )
      case Nil => visibleIndexes
    }

    def calculateVisibilityScore(row: List[Tree]): Int =
      ((row.drop(1).takeWhile(_.height < row.head.height).length + 1).min(row.length - 1))

    def calculateVisibilityScoreOfPosition(forest: List[List[Tree]], x: Int, y: Int) = {
      val rightSlice = forest(y).slice(x, forest(y).length)
      val leftSlice = forest(y).slice(0, x + 1).reverse
      val upSlice = forest.slice(0, y + 1).map(_(x)).reverse
      val downSlice = forest.slice(y, forest(y).length).map(_(x))

      val rightScore = calculateVisibilityScore(rightSlice)
      val leftScore = calculateVisibilityScore(leftSlice)
      val upScore = calculateVisibilityScore(upSlice)
      val downScore = calculateVisibilityScore(downSlice)

      rightScore *
        leftScore *
        upScore *
        downScore
    }

    def setVisibilityFromLeft(matrix: List[List[Tree]]) =
      matrix.zipWithIndex.map((row, idx) => {
        getIndexesOfVisiblePositions(row.map(_.height)).foldLeft(row)(
          (acc, visibleIndex) =>
            acc.updated(visibleIndex, acc(visibleIndex).copy(visible = true))
        )
      })

    val transformedTree = (0 to 3).foldLeft(grid) { (tree, iteration) =>
      rotateMatrix(setVisibilityFromLeft(tree))
    }

    printTreeMatrix(transformedTree)
    println("")
    println(s"No. of visible trees: ${transformedTree.flatten.filter(_.visible).length}")

    var highestScore = 0
    (0 to grid.length - 1).foreach(x => {
      (0 to grid(0).length - 1).foreach(y => {
        highestScore = calculateVisibilityScoreOfPosition(grid, x, y).max(highestScore)
      })
    })

    println(s"Highest visibility score: ${highestScore}")

  def rotateMatrix[T](matrix: List[List[T]]): List[List[T]] =
    matrix.transpose.map(_.reverse)

  def rotateMatrixBackwards[T](matrix: List[List[T]]): List[List[T]] =
    matrix.reverse.transpose

  def printTreeMatrix(matrix: List[List[Tree]]) =
    matrix.foreach { row =>
      row.foreach { tree =>
        print(
          (if (tree.visible) GREEN
           else RED) + tree.height + RESET
        )
      }
      print("\n")
    }
