package exercises

import util.DayN
import util.geometry.point.Point2D
import util.search.BFS

import scala.collection.immutable.SortedMap

object Day18 extends DayN {
  override val num = 18

  case class Tunnels(starts: Set[Point2D], path: Set[Point2D], walls: Set[Point2D], keys: Map[Point2D, Char], doors: Map[Point2D, Char])

  case class Key(distances: Map[Char, Int], keysRequired: Map[Char, Set[Char]])

  case class Data(start: Char, keysRequired: Map[Char, Set[Char]], distances: Map[(Char, Char), Int])

  def shortestRoute(data: Set[Data]): Int = {
    case class DataState(data: Data, hist: Map[(Char, Set[Char]), Int], waiting: List[DataLoopState], result: Option[Int])
    case class DataLoopState(key: Char, acquired: Set[Char], remaining: Set[Char], data: Data, dist: Int)
    case class LoopState(dataStates: List[(DataState, DataLoopState)], dist: Int)

    @annotation.tailrec
    def loopDs(states: SortedMap[Int, List[DataLoopState]], dataState: DataState): DataState = {
      if (states.isEmpty) dataState
      else {
        val dist = states.keys.head
        states(dist) match {
          case Nil => loopDs(states - dist, dataState)
          case state :: tail =>
            if (state.remaining.isEmpty)
              dataState.copy(waiting = List(state), result = Some(state.dist))
            else if (dataState.hist.get((state.key, state.acquired)).exists(_ <= state.dist))
              loopDs(states.updated(dist, tail), dataState)
            else {
              val possibles = state.remaining.filter(r => state.data.keysRequired(r).forall(state.acquired))
              if (possibles.isEmpty)
                loopDs(states.updated(dist, tail), dataState.copy(waiting = state :: dataState.waiting))
              else {
                val updated = states.updated(dist, tail)
                val possibleStates = possibles.map(p => DataLoopState(p, state.acquired + p, state.remaining - p, state.data, state.dist + state.data.distances(state.key, p)))
                val newStates = updated ++ possibleStates.groupBy(_.dist).map { case (k, v) => k -> (v.toList ++ updated.getOrElse(k, Nil)) }
                loopDs(newStates, dataState.copy(hist = dataState.hist + ((state.key, state.acquired) -> state.dist)))
              }
            }
        }
      }
    }

    @annotation.tailrec
    def loop(states: Set[LoopState]): Int = {
      val state = states.minBy(_.dist)
      if (state.dataStates.exists(_._1.result.isEmpty)) {
        // alot of this is bonkers attempts at dealing with waiting on doors to be opened from other Data blocks before I rage quit and "hacked" my input
        val newDStates = state.dataStates.map(ds => ds._2 -> {
          if (ds._1.result.nonEmpty) ds._1
          else loopDs(SortedMap(ds._2.dist -> List(ds._2)), ds._1.copy(waiting = Nil))
        })
        val combos = fliperoony(newDStates.map(ds => ds._2.waiting.map(ds._2 -> _)))
        val loopstates = combos.map(ds => {
          val acquired = ds.flatMap(_._2.acquired).toSet
          LoopState(
            ds.map { case (s, loopstate) => s -> loopstate.copy(acquired = acquired) },
            ds.map(_._2.dist).sum
          )
        })
        loop((states - state) ++ loopstates)
      } else {
        state.dataStates.flatMap(_._1.result).sum
      }
    }

    val initState = LoopState(data.map(d => (DataState(d, Map(), List(), None), DataLoopState('@', Set('@'), d.keysRequired.keySet - '@', d, 0))).toList, 0)
    loop(Set(initState))
  }

  def mapTunnels(tunnels: Tunnels): Set[Data] = {

    case class LoopState(p: Point2D, trail: Set[Point2D] = Set())

    val neighbours = tunnels.path.map(p => p -> p.neighbours.intersect(tunnels.path)).toMap

    def keyRequirements(p: Point2D, trail: Vector[Point2D]): Option[Set[Char]] =
      if (tunnels.keys.contains(p)) Some(trail.flatMap(d => tunnels.doors.get(d).map(_.toLower)).toSet)
      else None

    def keyDistance(p: Point2D, trail: Vector[Point2D]): Option[Int] =
      if (tunnels.keys.contains(p)) Some(trail.size) else None

    tunnels.starts.map(start => {
      val keys: Map[Char, Set[Char]] = BFS.fullSearch(start)(p => neighbours(p))(keyRequirements).map {
        case (k, v) => tunnels.keys(k) -> v
      }
      val keyDistances: Map[(Char, Char), Int] = (tunnels.keys.keySet + start).flatMap(p => {
        BFS.fullSearch(p)(p => neighbours(p))(keyDistance).map {
          case (k, v) => (tunnels.keys.getOrElse(p, '@'), tunnels.keys(k)) -> v
        }
      }).toMap
      Data('@', keys, keyDistances)
    })
  }

  private def parseInput(lines: List[String]): Tunnels = {
    val points = for {
      (r, y) <- lines.zipWithIndex
      (elem, x) <- r.zipWithIndex
    } yield Point2D(x, y) -> elem

    val starts = points.filter(_._2 == '@').map(_._1).toSet
    val path = points.filter(_._2 != '#').map(_._1).toSet
    val walls = points.filter(_._2 == '#').map(_._1).toSet
    val keys = points.filter(p => p._2 >= 'a' && p._2 <= 'z').toMap
    val doors = points.filter(p => p._2 >= 'A' && p._2 <= 'Z').toMap

    Tunnels(starts, path, walls, keys, doors)
  }

  private def modifyInput(tunnels: Tunnels): Tunnels = {
    val start = tunnels.starts.head
    val newStarts = start + Point2D(1, 1) :: start + Point2D(-1, -1) :: start + Point2D(1, -1) :: start + Point2D(-1, 1) :: Nil
    val newWalls = start.neighbours + start
    tunnels.copy(starts = newStarts.toSet, path = tunnels.path diff newWalls, walls = tunnels.walls ++ newWalls)
  }

  private def fliperoony[A](x: List[List[A]]): List[List[A]] = x match {
    case Nil => List(Nil)
    case h :: _ => h.flatMap(i => fliperoony(x.tail).map(i :: _))
  }

  val tunnels = parseInput(lines)
  val data = mapTunnels(tunnels)

  part1(shortestRoute(data))

  val modifiedTunnels = modifyInput(tunnels)
  val modifiedData = mapTunnels(modifiedTunnels)
  val hackedData = modifiedData.map(d => d.copy(keysRequired = d.keysRequired.mapValues(_.intersect(d.keysRequired.keySet))))

  part2(shortestRoute(hackedData))

}
