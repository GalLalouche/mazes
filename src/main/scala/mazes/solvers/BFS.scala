package mazes.solvers

import cats.syntax.functor.toFunctorOps
import mazes.data.{Direction, Grid}
import mazes.data.Grid.BFSGrid

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BFS:
  def apply(grid: Grid[Unit], x: Int, y: Int): BFSGrid = {
    @tailrec def go(queue: Queue[(Int, Int)], result: Grid[Option[Int]]): Grid[Option[Int]] = {
      queue.dequeueOption match
        case Some((x, y), newQueue) =>
          val cell = result(x, y)
          val value = cell.value.getOrElse(throw new AssertionError(s"No value for $cell"))
          val neighbors =
            for
              d <- Direction.values.toVector
              if cell.isLinkedOrTunneled(d)
              neighbor = cell.linkedOrTunneledNeighborUnsafe(d)
              if neighbor.value.isEmpty
            yield neighbor.coordinates
          val updatedValues =
            neighbors.foldLeft(result) {case (grid, (x, y)) => grid(x, y).set(Some(value + 1))}
          go(newQueue.enqueueAll(neighbors), updatedValues)
        case None => result
    }
    require(grid.applySafe(x, y).isDefined)
    val value = go(Queue(x -> y), grid.as(None)(x, y).set(Some(0)))
    value.map(_.get)
  }

  def challenging(grid: Grid[Int]): Grid[Int] = {
    val max = grid.coordinatesAndValues.maxBy(_._3)
    apply(grid.as(()), x = max._1, y = max._2)
  }