package mazes.graphics

import java.awt.{BasicStroke, Color}
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File

import javax.imageio.ImageIO
import mazes.data.{Direction, Grid}

import scala.language.unsafeNulls

object Drawing {
  def apply(grid: Grid, sideSize: Int = 100, output: String): Unit = {
    val borderDistance = sideSize / 2
    val imageWidth = sideSize * grid.width
    val imageHeight = sideSize * grid.height
    val image = new BufferedImage(
      imageWidth + borderDistance, imageHeight + borderDistance, BufferedImage.TYPE_INT_RGB)
    val g2d = image.createGraphics
    g2d.setColor(Color.WHITE)
    g2d.setStroke(new BasicStroke(10))
    g2d.fillRect(0, 0, imageWidth + borderDistance, imageHeight + borderDistance)
    g2d.setColor(Color.BLACK)

    // Vertically flip the image since (0,0) in the maze is at the bottom left but in the image
    // it is top left.
    val tx = AffineTransform.getScaleInstance(1, -1);
    tx.translate(0, -image.getHeight());
    g2d.transform(tx);
    // Translate to start from frame.
    g2d.translate(borderDistance / 2, borderDistance / 2)
    // Draw frame
    g2d.drawLine(0, 0, imageWidth, 0)
    g2d.drawLine(0, 0, 0, imageHeight)
    g2d.drawLine(imageWidth, 0, imageWidth, imageHeight)
    g2d.drawLine(0, imageHeight, imageWidth, imageHeight)

    for (x, y) <- grid.coordinates do
      val cell = grid(x, y)
      def draw(d: Direction, xSource: Int, ySource: Int): Unit =
        for
          otherCell <- cell.go(d)
          if !cell.isLinked(d)
        do
          val xTarget = (x + 1) * sideSize
          val yTarget = (y + 1) * sideSize
          g2d.drawLine(xSource * sideSize, ySource * sideSize, xTarget, yTarget)

      draw(Direction.East, xSource = x + 1, ySource = y)
      draw(Direction.North, xSource = x, ySource = y + 1)

    g2d.dispose()
    ImageIO.write(image, "png", new File(output))
  }
}
