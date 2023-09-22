package mazes.data

import cats.Functor
import cats.syntax.functor.toFunctorOps
import mazes.data.Grid.*

case class Grid[A] private(
    width: Int,
    height: Int,
    private val overLinks: Set[Link],
    private val underLinks: Set[Link],
    private val _values: IndexedSeq[A],
) {
  def tunnels = underLinks.map(l => (l.x, l.y, l.d.toString))
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
    def set(a: A): Grid[A] = Grid.this.copy(_values = _values.updated(translate(x, y), a))
    def goUnsafe(d: Direction): Cell =
      go(d).getOrElse(throw new IllegalArgumentException(s"Can't go from $this in direction $d"))
    def coordinates: (Int, Int) = (x, y)
    private def directionTo(cell: Grid[A]#Cell) = (cell.x - x, cell.y - y) match {
      case (1, 0) => Some(Direction.East)
      case (-1, 0) => Some(Direction.West)
      case (0, 1) => Some(Direction.North)
      case (0, -1) => Some(Direction.South)
      case _ => None
    }
    private def tunnelTo(cell: Grid[A]#Cell) = (cell.x - x, cell.y - y) match {
      case (2, 0) => Some(Direction.East)
      case (-2, 0) => Some(Direction.West)
      case (0, 2) => Some(Direction.North)
      case (0, -2) => Some(Direction.South)
      case _ => None
    }
    def linkToUnsafe(neighbor: Grid[A]#Cell): Grid[A] =
      directionTo(neighbor).map(linkUnsafe)
          .orElse(tunnelTo(neighbor).map(tunnelUnsafe))
          .getOrElse(throw new IllegalArgumentException(s"Cells $this and $neighbor aren't adjacent"))

    def directions: Seq[Direction] = Direction.values.filter(this.go(_).isDefined).toVector
    def neighbors: Seq[Cell] = Direction.values.flatMap(this.go).toVector
    def neighborsWithTunnels: Seq[Cell] = neighbors ++ tunnels
    private def tunnels: Seq[Cell] =
      for
        d <- Direction.values.toVector
        n <- go(d)
        if !isLinked(d) && !n.isLinked(d)
        if n.isOrthogonal(d)
        tunnelEnd <- n.go(d)
      yield tunnelEnd

    private def isOrthogonal(d: Direction) = d match
      case Direction.North | Direction.South => isLinked(Direction.East) && isLinked(Direction.West)
      case Direction.East | Direction.West => isLinked(Direction.North) && isLinked(Direction.South)

    private[Grid] def canonicalLink(d: Direction): Option[Link] =
      d match
        case Direction.North => go(d).as(Link(x, y, LinkDirection.Up))
        case Direction.East => go(d).as(Link(x, y, LinkDirection.Right))
        case Direction.West | Direction.South => go(d).flatMap(_.canonicalLink(d.opposite))
    private[Grid] def canonicalTunnel(d: Direction): Option[Link] =
      d match
        case Direction.North => go(d).flatMap(_.go(d)).as(Link(x, y, LinkDirection.Up))
        case Direction.East => go(d).flatMap(_.go(d)).as(Link(x, y, LinkDirection.Right))
        case Direction.West | Direction.South =>
          go(d).flatMap(_.go(d)).flatMap(_.canonicalTunnel(d.opposite))

    def isLinked(d: Direction): Boolean = canonicalLink(d).exists(overLinks)
    def isTunneled(d: Direction): Boolean = canonicalTunnel(d).exists(underLinks)
    def isLinkedOrTunneled(d: Direction): Boolean = isLinked(d) || isTunneled(d)
    def linkedOrTunneledNeighborUnsafe(d: Direction): Cell =
      if isLinked(d) then goUnsafe(d) else goUnsafe(d).ensuring(isTunneled(d)).goUnsafe(d)
    def link(d: Direction): Option[Grid[A]] =
      canonicalLink(d).map(l => Grid.this.copy(overLinks = overLinks + l))
    def linkUnsafe(d: Direction): Grid[A] =
      link(d).getOrElse(throw new IllegalArgumentException(s"Can't link $this in direction $d"))
    private def tunnelUnsafe(d: Direction): Grid[A] =
      canonicalTunnel(d)
          .map(l => Grid.this.copy(underLinks = underLinks + l))
          .getOrElse(throw new IllegalArgumentException(s"Can't link $this in direction $d"))
  def map[B](f: A => B): Grid[B] = copy(_values = _values.map(f))
}

object Grid:
  private val SideSize = 4
  enum LinkDirection {
    case Up, Right
  }
  private case class Link(x: Int, y: Int, d: LinkDirection)

  def apply(width: Int, height: Int): Grid[Unit] = new Grid(
    width = width,
    height = height,
    overLinks = Set.empty,
    underLinks = Set.empty,
    _values = Vector.fill(width * height)(())
  )

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
