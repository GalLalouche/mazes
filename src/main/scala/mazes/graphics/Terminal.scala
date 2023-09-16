package mazes.graphics

import mazes.data.{Direction, Grid}

object Terminal {
  def apply(grid: Grid): String = {
    val sb = StringBuilder()
    def bottomWall(y: Int) = {
      sb.append("+")
      for x <- 0 until grid.width do
        val e = if grid.applySafe(x, y).exists(_.isLinked(Direction.South)) then " " else "-"
        sb.append(e * SideSize).append("+")
      sb.append("\n")
    }
    bottomWall(grid.height)

    for y <- grid.height - 1 to 0 by -1 do
      sb.append("|")
      for x <- 0 until grid.width do
        sb.append(" " * SideSize)
        val cell = grid(x, y)
        sb.append(if cell.isLinked(Direction.East) then " " else "|")
      sb.append("\n")
      bottomWall(y)
    sb.toString
  }

  private val SideSize = 3
}
