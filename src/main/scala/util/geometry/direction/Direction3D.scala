package util.geometry.direction

import util.geometry.point.Point3D

sealed trait Direction3D {
  def mutation: Point3D
}

object Direction3D {
  def fromChar(c: Char): Direction3D = c match {
    case 'U' => Up3D
    case 'D' => Down3D
    case 'L' => Left3D
    case 'R' => Right3D
    case 'F' => Forwards3D
    case 'B' => Backwards3D
  }

  def all: Set[Direction3D] = Set(Left3D, Right3D, Up3D, Down3D, Forwards3D, Backwards3D)
}

case object Left3D extends Direction3D {
  val mutation = Point3D(-1, 0, 0)
}

case object Right3D extends Direction3D {
  val mutation = Point3D(1, 0, 0)
}

case object Up3D extends Direction3D {
  val mutation = Point3D(0, -1, 0)
}

case object Down3D extends Direction3D {
  val mutation = Point3D(0, 1, 0)
}

case object Forwards3D extends Direction3D {
  val mutation = Point3D(0, 0, 1)
}

case object Backwards3D extends Direction3D {
  val mutation = Point3D(0, 0, -1)
}

