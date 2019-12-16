package exercises

import intcode.{IntCode, State}
import util.{DayN, Point, Timer}

import scala.collection.immutable.Queue

object Day15 extends DayN {
  override val num = 15

  sealed trait Direction {
    def dir: Long
    def vec: Point
  }

  case object North extends Direction {
    val dir: Long = 1
    val vec = Point(0, 1)
  }

  case object South extends Direction {
    val dir: Long = 2
    val vec = Point(0, -1)
  }

  case object East extends Direction {
    val dir: Long = 3
    val vec = Point(1, 0)
  }

  case object West extends Direction {
    val dir: Long = 4
    val vec = Point(-1, 0)
  }

  object Direction {
    val all: Set[Direction] = Set(North, South, East, West)
  }

  case class Route(p: Point, dirs: List[Direction])

  def wanderToOxygen(state: State): Option[List[Direction]] =
    navigate(state)((r, _) => r.map(_.dirs))(_ == 2L)

  def wanderFully(state: State): Iterable[List[Direction]] =
    navigate(state)((_, m) => m.values)(_ => false)

  private def navigate[A](state: State)(f: (Option[Route], Map[Point, List[Direction]]) => A)(earlyExit: Long => Boolean): A = {
    @annotation.tailrec
    def loop(s: State, routeQueue: Queue[Route], history: Map[Point, List[Direction]]): A =
      routeQueue.dequeueOption match {
        case None => f(None, history)
        case Some((route, remaining)) =>
          val newState = moveSteps(s.copy(output = Queue()), route.dirs)
          newState.output.lastOption match {
            case None => f(None, history)
            case Some(output) => output match {
              case 0 => loop(s, remaining, history)
              case i if earlyExit(i) => f(Some(route), history)
              case _ =>
                val newRoutes = Direction.all.map(d => Route(route.p + d.vec, route.dirs :+ d)).filterNot(rs => {
                  history.get(rs.p).exists(_.size < rs.dirs.size)
                })
                loop(s, remaining ++ newRoutes, history + (route.p -> route.dirs))
            }
          }
      }

    loop(state, Queue() ++ Direction.all.map(d => Route(d.vec, List(d))), Map(Point(0, 0) -> Nil))
  }

  private def moveSteps(state: State, directions: List[Direction]): State =
    IntCode.run(state.copy(input = state.input ++ directions.map(_.dir)))

  private def point(directions: List[Direction]): Point = directions.foldLeft(Point(0, 0))((b, d) => b + d.vec)

  val input: Map[Long, Long] = IntCode.parseInput(lines.head)
  val init = State(0, input, Queue())

  val routeToOxygen = wanderToOxygen(init)
  part1(routeToOxygen.map(_.size))
  part2(wanderFully(moveSteps(init, routeToOxygen.get)).map(_.size).max)
}
