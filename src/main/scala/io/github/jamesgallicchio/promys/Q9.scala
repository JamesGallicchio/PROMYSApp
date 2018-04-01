package io.github.jamesgallicchio.promys

import scala.collection.parallel.immutable.ParSeq
import scala.concurrent.Future

object Q9 {

  def main(args: Array[String]): Unit = {
    /*
      val (bestestS, bestestC) = (0 to 0).par.map { _ =>

      }.filter(_.forall(!_._1.isNaN))
        .foldLeft((Seq.empty[(Double, Double)], Double.PositiveInfinity)) {
          case ((bestS, bestC), s) => val c = cost(s.map(_._1))
            if (c < bestC) {println(s); println(c); (s, c)} else (bestS, bestC)
        }
*/

    // val start = Seq((0.0,0.125), (0.3,0.25), (0.7, 0.25), (2.0, 1.0), (4.0, 1.0), (100.0, 256.0))

    for (i <- 3 to 20) {
      val start = (0 until i).map(_.toDouble).map((_, 0.5))

      val (bestS, bestC) = refine(start, 10000000)

      println(s"\n## $i ##")
      println(s"Best: ${1.0/bestC}")
      println(bestS.map(_._1).mkString(", "))

    }
  }

  def refine(start: Seq[(Double, Double)], iter: Int): (Seq[(Double, Double)], Double) =
    (0 to iter).foldLeft((start, Double.MaxValue)) { case ((lastS, lastC), _) =>
      val nums = lastS.map(_._1)
      val c = cost(nums)

      if (Math.abs(lastC-c) < 0.0000000000000001) return (lastS, c)

      val nextS = lastS.zipWithIndex.map { case ((e, dX), i) =>

        val dC = cost(nums.updated(i, relu(e + dX))) - c

        val nDX = if (dC == 0) dX else if (dC < 0) 2.0*dX else -dX / 2.0

        (relu(e + nDX), nDX)
      }.seq

      (nextS, c)
    }

  def func(x: Double, y: Double) = Math.abs(x - y) / (1 + x * y)

  def cost(nums: Seq[Double]): Double =
    1.0/nums.combinations(2).map{case Seq(x, y) => func(x, y) }.min

  def lim(x: Double): Double = if (x == 0.0) 0.0 else if (x > 0.0) 1.0 else -1.0

  def relu(x: Double): Double = if (x < 0.0) 0.0 else x

  val minShrink = 0.000000001
  def shrink(x: Double): Double = if (x < 0) (x-minShrink)/2 else (x+minShrink)/2
}
