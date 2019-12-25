package util.geometry.point

import util.geometry.direction.{Direction2D, Direction3D}

case class Point3D(x: Int, y: Int, z: Int) {
  private def mutate(p: Point3D)(op: (Int, Int) => Int): Point3D = Point3D(op(x, p.x), op(y, p.y), op(z, p.z))
  private def mutate(p: Point2D)(op: (Int, Int) => Int): Point3D = Point3D(op(x, p.x), op(y, p.y), z)

  def +(p: Point3D): Point3D = mutate(p)(_ + _)

  def -(p: Point3D): Point3D = mutate(p)(_ - _)

  def *(p: Point3D): Point3D = mutate(p)(_ * _)

  def /(p: Point3D): Point3D = mutate(p)(_ / _)

  def +(p: Point2D): Point3D = mutate(p)(_ + _)

  def -(p: Point2D): Point3D = mutate(p)(_ - _)

  def *(p: Point2D): Point3D = mutate(p)(_ * _)

  def /(p: Point2D): Point3D = mutate(p)(_ / _)

  def *(i: Int): Point3D = mutate(Point3D(i, i, i))(_ * _)

  def manHattanDist(p: Point3D): Int = math.abs(x - p.x) + math.abs(y - p.y) + math.abs(z - p.z)

  def dropZ: Point2D = Point2D(x, y)

  def to(target: Point3D, direction: Direction3D): List[Point3D] = {
    @annotation.tailrec
    def loop(curr: Point3D, acc: List[Point3D]): List[Point3D] = {
      curr match {
        case `target` => acc
        case p =>
          val next = p + direction.mutation
          loop(next, next :: acc)
      }
    }

    loop(this, Nil).reverse
  }

  def neighbours: Set[Point3D] = Direction3D.all.map(_.mutation + this)
  def neighbours2d: Set[Point3D] = Direction2D.all.map(_.mutation.addZ(0) + this)
}
