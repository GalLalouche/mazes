package mazes.builders

import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.effect.IO
import mazes.data.Grid
import mazes.solvers.BFS
import org.scalacheck.{Gen, Shrink}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.annotation.experimental

@experimental class MazeGeneratorTest extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
  import mazes.builders.MazeGeneratorTest.Fixture

  private implicit val shrinker: Shrink[Fixture] = MazeGeneratorTest.ShrinkFixture
  private val generators = Vector(
    BinaryTree,
    Sidewinder,
    RandomWalk,
    RandomWalk.weavingGenerator,
    HuntAndKill,
    HuntAndKill.weavingGenerator,
    Backtracker,
    Backtracker.weavingGenerator,
  )
  generators.foreach(generator => property(s"$generator: Should always be solvable") {
    forAll(MazeGeneratorTest.GenFixture) {case Fixture(width, height, x0, y0, seed) =>
      // TODO Use proper cats testing
      implicit val random: Random[IO] = Random.scalaUtilRandomSeedLong[IO](seed).unsafeRunSync()
      val grid: Grid[Unit] = generator[IO](width, height).unsafeRunSync()
      noException shouldBe thrownBy {BFS(grid, x0, y0)}
    }
  })
}

object MazeGeneratorTest {
  private case class Fixture(width: Int, height: Int, x0: Int, y0: Int, seed: Long)

  private val MaxSide = 20
  private val MinSide = 2
  private implicit val GenFixture: Gen[Fixture] = for
    width <- Gen.choose(MinSide, MaxSide)
    height <- Gen.choose(MinSide, MaxSide)
    x0 <- Gen.choose(0, width - 1)
    y0 <- Gen.choose(0, height - 1)
    seed <- Gen.long
  yield Fixture(width, height, x0, y0, seed)

  private val ShrinkFixture: Shrink[Fixture] =
    Shrink.xmap[(Int, Int, Int, Int, Long), Fixture](
      Fixture.apply.tupled,
      f => (f.width, f.height, f.x0, f.y0, f.seed)
    )(Shrink.shrinkTuple5[Int, Int, Int, Int, Long].suchThat(
      e => e._1 >= MinSide &&
          e._2 >= MinSide &&
          e._3 >= 0 && e._3 < e._1 &&
          e._4 >= 0 && e._4 < e._2))
}