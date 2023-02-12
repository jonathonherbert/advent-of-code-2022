package advent

// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  test("taxicab distance") {
    assertEquals(1, day12.getDistance(Pos(0, 12), Pos(0, 11)))
    assertEquals(1, day12.getDistance(Pos(0, 11), Pos(0, 12)))
    assertEquals(2, day12.getDistance(Pos(1, 12), Pos(0, 11)))
    assertEquals(2, day12.getDistance(Pos(-1, 12), Pos(0, 11)))
  }

  test("valid moves") {
    val grid = List(
      List('b', 'a', 'a', 'b'),
      List('b', 'b', 'b', 'a'),
      List('c', 'c', 'a', 'a'),
      List('c', 'c', 'd', 'd')
    )

    assertEquals(
      day12.getValidMoves(
        Pos(3, 0),
        Set(Pos(3, 0)),
        grid
      ),
      List(
        Mv(dir = 'v', pos = Pos(x = 3, y = 1)),
        Mv(dir = '<', pos = Pos(x = 2, y = 0))
      )
    )

    assertEquals(
      day12.getValidMoves(
        Pos(1, 2),
        Set.empty,
        grid
      ),
      List(
        Mv(dir = 'v', pos = Pos(x = 1, y = 3)),
        Mv(dir = '>', pos = Pos(x = 2, y = 2)),
        Mv(dir = '<', pos = Pos(x = 0, y = 2)),
        Mv(dir = '^', pos = Pos(x = 1, y = 1))
      )
    )
  }
}
