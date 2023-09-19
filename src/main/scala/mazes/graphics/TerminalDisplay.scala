package mazes.graphics

import mazes.data.{Direction, Grid}

object TerminalDisplay:
  def apply[A](grid: Grid[A])(using ev: Show[A]): String = {
    val sb = StringBuilder()
    def horizontalWall(y: Int) = {
      val mid = if y == 0 || y == grid.height then "+" else "|"
      sb.append(mid)
      for x <- 0 until grid.width do
        val e = if grid.applySafe(x, y).exists(_.isLinked(Direction.South)) then " " else "="
        sb.append(e * SideSize).append(mid)
      sb.append("\n")
    }

    horizontalWall(grid.height)

    for y <- grid.height - 1 to 0 by -1 do
      sb.append("|")
      for x <- 0 until grid.width do
        val cell = grid(x, y)
        sb.append(ev.show(cell.value))
        sb.append(if cell.isLinked(Direction.East) then " " else "|")
      sb.append("\n")
      horizontalWall(y)

    sb.toString
  }

  trait Show[A]:
    def show(a: A): String
  given Show[Unit] with
    def show(a: Unit): String = " " * SideSize
  given Show[Int] with
    def show(value: Int): String = s"%0${SideSize}d".format(value)
  given Show[Boolean] with
    def show(value: Boolean): String = s" ${value.toString.head} "

  private val SideSize = 3
