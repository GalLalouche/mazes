package mazes.data

import cats.Functor
import cats.syntax.functor.toFunctorOps
import mazes.data.Grid.*

class Grid[A] private(
    val width: Int, val height: Int, openings: Set[Link], _values: IndexedSeq[A]) {
  val size: Int = _values.size
  val values: Iterable[A] = _values
  def apply(x: Int, y: Int): Cell = Cell(x, y)
  def applySafe(x: Int, y: Int): Option[Cell] =
    if x >= 0 && x < width && y >= 0 && y < height then Some(apply(x, y)) else None

  def coordinates: Seq[(Int, Int)] = Grid.coordinates(height = height, width = width)
  def coordinatesAndValues: Iterator[(Int, Int, A)] =
    coordinates.iterator.map((x, y) => (x, y, _values(translate(x, y))))
  def cells: Seq[Cell] = coordinates.map(Cell.apply)
  private def translate(x: Int, y: Int): Int = x * height + y

  case class Cell(x: Int, y: Int):
    assert(x >= 0 && x < width && y >= 0 && y < height, s"($x, $y) was not in grid!")
    def go(d: Direction): Option[Cell] =
      d match
        case Direction.North => if y < height - 1 then Some(Cell(x, y + 1)) else None
        case Direction.South => if y > 0 then Some(Cell(x, y - 1)) else None
        case Direction.West => if x > 0 then Some(Cell(x - 1, y)) else None
        case Direction.East => if x < width - 1 then Some(Cell(x + 1, y)) else None
    def value: A = _values(translate(x, y))
    def set(a: A): Grid[A] = new Grid(width, height, openings, _values.updated(translate(x, y), a))
    def goUnsafe(d: Direction): Cell =
      go(d).getOrElse(throw new IllegalArgumentException(s"Can't go from $this in direction $d"))
    def coordinates: (Int, Int) = (x, y)
    def directionToUnsafe(cell: Grid[A]#Cell): Direction = (cell.x - x, cell.y - y) match {
      case (1, 0) => Direction.East
      case (-1, 0) => Direction.West
      case (0, 1) => Direction.North
      case (0, -1) => Direction.South
      case _ => throw new IllegalArgumentException(s"Cells $this and $cell aren't adjacent")
    }
    def linkToUnsafe(neighbor: Grid[A]#Cell): Grid[A] = linkUnsafe(directionToUnsafe(neighbor))

    def directions: Seq[Direction] = Direction.values.filter(this.go(_).isDefined).toVector
    def neighbors: Seq[Cell] = Direction.values.flatMap(this.go).toVector

    private[Grid] def canonical(d: Direction): Option[Link] =
      d match
        case Direction.North => go(d).as(Link(x, y, LinkDirection.Up))
        case Direction.East => go(d).as(Link(x, y, LinkDirection.Right))
        case Direction.West | Direction.South => go(d).flatMap(_.canonical(d.opposite))

    def isLinked(d: Direction): Boolean = canonical(d).exists(openings)
    def link(d: Direction): Option[Grid[A]] =
      canonical(d).map(l => new Grid(width, height, openings + l, _values))
    def linkUnsafe(d: Direction): Grid[A] =
      link(d).getOrElse(throw new IllegalArgumentException(s"Can't link $this in direction $d"))
  def map[B](f: A => B): Grid[B] = new Grid(width, height, openings, _values.map(f))
}

object Grid:
  private val SideSize = 4
  enum LinkDirection {
    case Up, Right
  }
  private case class Link(x: Int, y: Int, d: LinkDirection)

  def apply(width: Int, height: Int): Grid[Unit] =
    new Grid(width, height, Set.empty, Vector.fill(width * height)(()))

  def coordinates(height: Int, width: Int): Seq[(Int, Int)] =
    for
      y <- 0 until height
      x <- 0 until width
    yield
      (x, y)

  given Functor[Grid] with
    override def map[A, B](fa: Grid[A])(f: A => B): Grid[B] = fa.map(f)

  type BasicGrid = Grid[Unit]
  type BFSGrid = Grid[Int]
