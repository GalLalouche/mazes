package mazes.builders

import cats.Monad
import cats.data.{IndexedStateT, StateT}
import cats.effect.std.Random
import cats.implicits.toFunctorOps
import cats.syntax.flatMap.*
import mazes.builders.WalkerSt.{GCell, GridB}
import mazes.data.Grid
import mazes.data.Grid.BasicGrid
import mazes.utils.func.RandomUtils

import scala.annotation.experimental

@experimental case object Backtracker extends WeavingMazeGeneratorTemplate {
  import language.experimental.namedTypeArguments

  private type Path = List[(Int, Int)]
  protected override def apply[F[_]: Monad: Random](
      width: Int, height: Int, getNeighbors: GCell => Seq[GCell]
  ): F[BasicGrid] = {
    type State = (GCell, GridB)
    type Ret = StateT[F, Path, State]
    object Aux extends Walker[StateT[F, Path, _]] {
      override protected def template(grid: GridB, src: GCell): Ret =
        StateT.modify[F, Path]((src.x, src.y) :: _) >> go(grid)
      def go(result: Grid[Boolean]): Ret = {
        for
          stack <- StateT.get[F, Path]
          head :: tail = stack: @unchecked
          cell = result.apply.tupled(head)
          neighbors = getNeighbors(cell).filterNot(_.value)
          rec <-
            if neighbors.isEmpty // Pop and try again
            then StateT.set[F, Path](tail) >> go(result)
            // Weird bug! If I use map, this works, but if I use fproduct, I have to explicitly
            // state the liftF type!
            else StateT.liftF[A = State
            ] (RandomUtils.sampleUnsafe(neighbors).fproduct(link(cell, _)))
        yield rec
      }
    }
    Aux(width, height).runA(Nil)
  }
}