package util

object Timer {
  def time[A](f: => A, msg: String = ""): A = {
    val start = System.currentTimeMillis()
    val result = f
    println(s"$msg${System.currentTimeMillis() - start}ms")
    result
  }
}
