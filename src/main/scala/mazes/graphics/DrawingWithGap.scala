package mazes.graphics

import java.awt.{BasicStroke, Color}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import mazes.data.{Direction, Grid}
import mazes.data.Direction.{East, North, South, West}
import mazes.data.Grid.BasicGrid

import scala.language.unsafeNulls

object DrawingWithGap {
  def apply(grid: BasicGrid, output: String): Unit = {
    println(grid.tunnels)
    val borderDistance = 0
    val imageWidth = SideSize * grid.width
    val imageHeight = SideSize * grid.height
    val image = new BufferedImage(
      imageWidth + borderDistance, imageHeight + borderDistance, BufferedImage.TYPE_INT_RGB)
    val g2d = image.createGraphics
    g2d.setColor(Color.WHITE)
    g2d.setStroke(new BasicStroke(Stroke))
    g2d.fillRect(0, 0, imageWidth + borderDistance, imageHeight + borderDistance)

    // Vertically flip the image since (0,0) in the maze is at the bottom left but in the image
    // it is top left.
    val tx = AffineTransform.getScaleInstance(1, -1)
    tx.translate(0, -image.getHeight())
    g2d.transform(tx)
    val original = g2d.getTransform
    // Translate to start from frame.

    for (x, y) <- grid.coordinates do
      val cell = grid(x, y)

      g2d.setColor(Color.BLACK)
      g2d.setTransform(original)
      g2d.translate(x * SideSize, y * SideSize)
      for
        d <- Direction.values
        if !cell.isLinkedOrTunneled(d)
      do
        val xSrc = d match
          case North | South => if cell.isLinkedOrTunneled(Direction.West) then FarStart else NearStart
          case West => NearStart
          case East => NearEnd
        val xDst = d match
          case North | South => if cell.isLinkedOrTunneled(Direction.East) then FarEnd else NearEnd
          case West => NearStart
          case East => NearEnd
        val ySrc = d match
          case West | East => if cell.isLinkedOrTunneled(Direction.South) then FarStart else NearStart
          case North => NearEnd
          case South => NearStart
        val yDst = d match
          case West | East => if cell.isLinkedOrTunneled(Direction.North) then FarEnd else NearEnd
          case North => NearEnd
          case South => NearStart
        g2d.drawLine(xSrc, ySrc, xDst, yDst)

      // Close off wall adjacent holes.
      if cell.isLinkedOrTunneled(East) then
        g2d.drawLine(NearEnd, NearEnd, FarEnd, NearEnd)
        g2d.drawLine(NearEnd, NearStart, FarEnd, NearStart)
      if cell.isLinkedOrTunneled(West) then
        g2d.drawLine(NearStart, NearEnd, FarStart - Offset, NearEnd)
        g2d.drawLine(NearStart, NearStart, FarStart - Offset, NearStart)
      if cell.isLinkedOrTunneled(North) then
        g2d.drawLine(NearEnd, NearEnd, NearEnd, FarEnd)
        g2d.drawLine(NearStart, NearEnd, NearStart, FarEnd + Offset)
      if cell.isLinkedOrTunneled(South) then
        g2d.drawLine(NearEnd, NearStart, NearEnd, FarStart - Offset)
        g2d.drawLine(NearStart, NearStart, NearStart, FarStart)

    g2d.dispose()
    ImageIO.write(image, "png", new File(output))
  }

  private val SideSize = 100
  private val Offset = SideSize / 10
  private val FarStart = 0
  private val NearStart = Offset
  private val NearEnd = SideSize - Offset
  private val FarEnd = SideSize
  private val Stroke: Float = 10f
}
