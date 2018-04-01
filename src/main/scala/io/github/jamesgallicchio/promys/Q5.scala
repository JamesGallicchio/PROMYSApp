package io.github.jamesgallicchio.promys

import scala.annotation.tailrec

object Q5 {
  def main(args: Array[String]): Unit = {

    testGeneralized
  }

  def testGeneralized = {

    val primes = Seq(2, 3, 5, 7, 11, 13, 17, 19, 23)
    for (pCount <- 1 to primes.length) {
      val s = primes.take(pCount)
      val product = s.product

      val terms = for {
        i <- 0 to s.length
        num = product * (if (i%2 == 0) 1 else -1)
        den <- s.combinations(i)
      } yield num/den.product

      def gcd(x: Int, y: Int): Int = {

        @tailrec
        def rec(a: Int, b: Int): Int = a % b match {
          case 0 => b
          case rem => rec(b, rem)
        }

        rec(x, y)
      }
      val nPreGCD = terms.sum
      val g = gcd(nPreGCD, product)
      val n = nPreGCD/g
      val d = product/g

      println(s"$n/$d = ${n.toDouble/d}")
    }

  }

  def testQuiteprime(n: Int = 1000000000) = {
    println(
      (1 until n).par.count(i => i%2!=0 && i%3!=0 && i%5!=0)
    )
    println(n-n/2-n/3-n/5+n/6+n/10+n/15-n/30)
  }

  def testVeryquiteprime(n: Int = 30030) = {
    println(
      (1 until n).par.count(i => i%2!=0 && i%3!=0 && i%5!=0 && i%7!=0 && i%11!=0 && i%13!=0)
    )
  }
}
