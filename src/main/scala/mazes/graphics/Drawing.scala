package mazes.graphics

import java.awt.{BasicStroke, Color}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File

import cats.implicits.toFunctorOps
import javax.imageio.ImageIO
import mazes.data.{Direction, Grid}
import mazes.data.Direction.{East, North, South, West}
import mazes.data.Grid.{BasicGrid, BFSGrid}

import scala.language.unsafeNulls
import scala.util.Random

object Drawing {
  def apply(grid: BasicGrid, output: String, withGap: Boolean): Unit = {
    apply(grid.as(0), output, withGap, _ => Color.WHITE)
  }
  def applyBFS(grid: BFSGrid, output: String, withGap: Boolean): Unit = {
    apply(grid, output, withGap, colorChooser())
  }
  private def apply(
      grid: BFSGrid, output: String, withGap: Boolean, colorChooser: Float => Color
  ): Unit = {
    val max = math.max(grid.values.view.max, 1)
    def color(cell: BFSGrid#Cell): Color = colorChooser(cell.value.toFloat / max)
    val offset = if withGap then Gap else 0
    val offsetPlus = (offset * 1.5).toInt
    val farStart = 0
    val nearStart = offset
    val nearEnd = SideSize - offset
    val farEnd = SideSize

    val borderDistance = if withGap then 0 else SideSize / 5
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
    g2d.translate(borderDistance / 2, borderDistance / 2) // Translate to start from frame.
    val original = g2d.getTransform

    for (x, y) <- grid.coordinates do
      val cell = grid(x, y)

      g2d.setColor(Color.BLACK)
      g2d.setTransform(original)
      g2d.translate(x * SideSize, y * SideSize)
      val cellColor = color(cell)
      g2d.setColor(cellColor)
      g2d.fillRect(nearStart, nearStart, SideSize - offset * 2, SideSize - offset * 2)
      g2d.setColor(Color.BLACK)
      for
        d <- Direction.values
        if !cell.isLinkedOrTunneled(d)
      do
        val xSrc = d match
          case North | South => if cell.isLinkedOrTunneled(Direction.West) then farStart else nearStart
          case West => nearStart
          case East => nearEnd
        val xDst = d match
          case North | South => if cell.isLinkedOrTunneled(Direction.East) then farEnd else nearEnd
          case West => nearStart
          case East => nearEnd
        val ySrc = d match
          case West | East => if cell.isLinkedOrTunneled(Direction.South) then farStart else nearStart
          case North => nearEnd
          case South => nearStart
        val yDst = d match
          case West | East => if cell.isLinkedOrTunneled(Direction.North) then farEnd else nearEnd
          case North => nearEnd
          case South => nearStart
        g2d.drawLine(xSrc, ySrc, xDst, yDst)

      if withGap then
        // Close off wall adjacent holes, color gaps
        if cell.isLinkedOrTunneled(East) then
          val neighborColor = color(cell.linkedOrTunneledNeighborUnsafe(East))
          val gapColor = averageColor(cellColor, neighborColor)
          g2d.setColor(gapColor)
          g2d.fillRect(nearEnd, nearStart, offset * 2, SideSize - 2 * offset)

          g2d.setColor(Color.BLACK)
          g2d.drawLine(nearEnd, nearEnd, farEnd, nearEnd)
          g2d.drawLine(nearEnd, nearStart, farEnd, nearStart)
        if cell.isLinkedOrTunneled(West) then
          val neighborColor = color(cell.linkedOrTunneledNeighborUnsafe(West))
          val gapColor = averageColor(cellColor, neighborColor)
          g2d.setColor(gapColor)
          g2d.fillRect(nearStart - offsetPlus, nearStart, offsetPlus, SideSize - 2 * offset)

          g2d.setColor(Color.BLACK)
          g2d.drawLine(nearStart, nearEnd, farStart - offset, nearEnd)
          g2d.drawLine(nearStart, nearStart, farStart - offset, nearStart)
        if cell.isLinkedOrTunneled(North) then
          val neighborColor = color(cell.linkedOrTunneledNeighborUnsafe(North))
          val gapColor = averageColor(cellColor, neighborColor)
          g2d.setColor(gapColor)
          g2d.fillRect(nearStart, nearEnd, SideSize - 2 * offset, offset * 2)

          g2d.setColor(Color.BLACK)
          g2d.drawLine(nearEnd, nearEnd, nearEnd, farEnd)
          g2d.drawLine(nearStart, nearEnd, nearStart, farEnd + offset)
        if cell.isLinkedOrTunneled(South) then
          val neighborColor = color(cell.linkedOrTunneledNeighborUnsafe(South))
          val gapColor = averageColor(cellColor, neighborColor)
          g2d.setColor(gapColor)
          g2d.fillRect(nearStart, nearStart - offsetPlus, SideSize - 2 * offset, offsetPlus)

          g2d.setColor(Color.BLACK)
          g2d.drawLine(nearEnd, nearStart, nearEnd, farStart - offset)
          g2d.drawLine(nearStart, nearStart, nearStart, farStart)

    g2d.dispose()
    ImageIO.write(image, "png", new File(output))
  }

  private def colorChooser(): Float => Color = {
    val baseColor = Color.getHSBColor(Random.nextFloat(), 1, 1)
    val hsb: Seq[Float] = {
      val array = Drawing.getHsb(baseColor)
      array(1) = 1
      array(2) = 1
      array.toVector
    }
    // The very low numbers (where we begin the maze) are close to white. The very high numbers
    // (which are farther away from the beginning) are close to black. Between them, we go through
    // the random hue.
    val minColorValue = 0.05f
    val maxColorValue = 0.9f
    def ratio(r: Float) = {
      val range = maxColorValue - minColorValue
      assert(range > 0)
      minColorValue + r * range
    }
    r => Color.getHSBColor(hsb(0), ratio(r), 1 - ratio(r))
  }

  private val SideSize = 100
  private val Gap = SideSize / 10
  private val Stroke: Float = 10f

  private def averageColor(c1: Color, c2: Color): Color = {
    val hsb1 = getHsb(c1)
    val hsb2 = getHsb(c2)
    def avg(i: Int): Float = (hsb1(i) + hsb2(i)) / 2f
    Color.getHSBColor(avg(0), avg(1), avg(2))
  }

  private def getHsb(color: Color): Array[Float] = {
    // Fucking Java.
    val array = new Array[Float](3)
    Color.RGBtoHSB(color.getRed, color.getGreen, color.getBlue, array)
    array
  }
}
