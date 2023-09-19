package mazes.graphics

import java.awt.{BasicStroke, Color}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File

import cats.implicits.toFunctorOps
import javax.imageio.ImageIO
import mazes.data.{Direction, Grid}
import mazes.data.Grid.{BasicGrid, BFSGrid}

import scala.language.unsafeNulls
import scala.util.Random

object Drawing {
  def apply(grid: BasicGrid, output: String): Unit = {
    applyBFS(grid.as(0), output, _ => Color.WHITE)
  }

  def apply(grid: BFSGrid, output: String)(implicit dummy: DummyImplicit): Unit = {
    val baseColor = Color.getHSBColor(Random.nextFloat(), 1, 1)
    val hsb: Seq[Float] = {
      // Fucking Java.
      val array = new Array[Float](3)
      Color.RGBtoHSB(baseColor.getRed, baseColor.getGreen, baseColor.getBlue, array)
      array(1) = 1
      array(2) = 1
      array.toVector
    }
    // The very low numbers (where we begin the maze) are close to white. The very high numbers
    // (which are farther away from the beginning) are close to black. Between them, we go through
    // the random hue.
    val minColorValue = 0.05f
    val maxColorValue = 0.95f
    def ratio(r: Float) = {
      val range = maxColorValue - minColorValue
      assert(range > 0)
      minColorValue + r * range
    }
    def color(r: Float): Color = Color.getHSBColor(hsb(0), ratio(r), 1 - ratio(r))
    applyBFS(grid, output, color)
  }

  private def applyBFS(grid: BFSGrid, output: String, color: Float => Color): Unit = {
    val borderDistance = SideSize / 2
    val imageWidth = SideSize * grid.width
    val imageHeight = SideSize * grid.height
    val image = new BufferedImage(
      imageWidth + borderDistance, imageHeight + borderDistance, BufferedImage.TYPE_INT_RGB)
    val g2d = image.createGraphics
    g2d.setColor(Color.WHITE)
    g2d.setStroke(new BasicStroke(Stroke))
    // Draw frame
    g2d.fillRect(0, 0, imageWidth + borderDistance, imageHeight + borderDistance)

    // Vertically flip the image since (0,0) in the maze is at the bottom left but in the image
    // it is top left.
    val tx = AffineTransform.getScaleInstance(1, -1);
    tx.translate(0, -image.getHeight());
    g2d.transform(tx);
    // Translate to start from frame.
    g2d.translate(borderDistance / 2, borderDistance / 2)
    val max = math.max(grid.values.view.max, 1)
    // Fucking java.

    for (x, y) <- grid.coordinates do
      val cell = grid(x, y)
      def draw(d: Direction, xSource: Int, ySource: Int): Unit =
        for
          otherCell <- cell.go(d)
          if !cell.isLinked(d)
        do
          val xTarget = (x + 1) * SideSize
          val yTarget = (y + 1) * SideSize
          g2d.drawLine(xSource * SideSize, ySource * SideSize, xTarget, yTarget)

      g2d.setColor(color(cell.value.toFloat / max))
      g2d.fillRect(
        x * SideSize + (Stroke / 2).toInt, y * SideSize + (Stroke / 2).toInt, SideSize, SideSize)
      g2d.setColor(Color.BLACK)
      draw(Direction.East, xSource = x + 1, ySource = y)
      draw(Direction.North, xSource = x, ySource = y + 1)

    g2d.drawLine(0, 0, imageWidth, 0)
    g2d.drawLine(0, 0, 0, imageHeight)
    g2d.drawLine(imageWidth, 0, imageWidth, imageHeight)
    g2d.drawLine(0, imageHeight, imageWidth, imageHeight)

    g2d.dispose()
    ImageIO.write(image, "png", new File(output))
  }

  private val SideSize = 100
  private val Stroke: Float = 10f
}
