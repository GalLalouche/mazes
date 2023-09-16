package mazes.data

import cats.syntax.functor.toFunctorOps
import mazes.data.Grid.*

class Grid private(val width: Int, val height: Int, openings: Set[Link]) {
  def this(width: Int, height: Int) = this(width, height, Set.empty)
  def apply(x: Int, y: Int): Cell = Cell(x, y)
  def applySafe(x: Int, y: Int) =
    if x >= 0 && x < width && y >= 0 && y < height then Some(apply(x, y)) else None

  def coordinates: Seq[(Int, Int)] = Grid.coordinates(height = height, width = width)

  case class Cell(x: Int, y: Int):
    assert(x >= 0 && x < width && y >= 0 && y < height, s"($x, $y) was not in grid!")
    def go(d: Direction): Option[Cell] =
      d match
        case Direction.North => if y < height - 1 then Some(Cell(x, y + 1)) else None
        case Direction.South => if y > 0 then Some(Cell(x, y - 1)) else None
        case Direction.West => if x > 0 then Some(Cell(x - 1, y)) else None
        case Direction.East => if x < width - 1 then Some(Cell(x + 1, y)) else None
    def goUnsafe(d: Direction): Cell =
      go(d).getOrElse(throw new IllegalArgumentException(s"Can't go from $this in direction $d"))

    private[Grid] def canonical(d: Direction): Option[Link] =
      d match
        case Direction.North => go(d).as(Link(x, y, LinkDirection.Up))
        case Direction.East => go(d).as(Link(x, y, LinkDirection.Right))
        case Direction.West | Direction.South => go(d).flatMap(_.canonical(d.opposite))

    def isLinked(d: Direction): Boolean = canonical(d).exists(openings)
    def link(d: Direction): Option[Grid] =
      canonical(d).map(l => Grid(width, height, openings + l))
    def linkUnsafe(d: Direction): Grid =
      link(d).getOrElse(throw new IllegalArgumentException(s"Can't link $this in direction $d"))
}

object Grid:
  private val SideSize = 4
  enum LinkDirection {
    case Up, Right
  }
  private case class Link(x: Int, y: Int, d: LinkDirection)

  def coordinates(height: Int, width: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield
      (x, y)
