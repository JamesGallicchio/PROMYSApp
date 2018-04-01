package io.github.jamesgallicchio.promys

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, TreeSet}
import scala.util.Sorting

/**
  * Created by JGallicchio on 3/13/2018.
  */
object Q4 {

  def main(args: Array[String]): Unit = {
    @tailrec
    def rec(iters: Int, acc: Set[Int], prevGen: Set[Int]): Set[Int] = {
      println(s"$iters --- ${acc.size}")
      // Flip previously generated values
      val prevFlipped = prevGen.par.map(r => r.denom /: (2*r.numer)).seq
      // Combine previous generated with flipped versions (total previous generation)
      val prev = prevGen | prevFlipped

      val newAcc = acc | prev

      iters match {
        // If out of iterations return all generated rationals
        case 0 => newAcc

        // Otherwise combine prev to (acc | prev)
        case x =>
          val combined = for {
            r1 <- prev.par
            r2 <- newAcc
          } yield (r1.numer + r2.numer) /: (r1.denom + r2.denom)
          rec(x-1, newAcc, combined.seq)
      }
    }

    @tailrec
    def next(x: Int, y: Int): (Int, Int) =
      if (x+1 > y) next(y/2, y+1)
      else if (gcd(x+1, y) != 1) next(x+1, y)
      else (x+1, y)

    @tailrec
    def check(s: Iterator[Int], exp: Iterator[(Int, Int)] = Iterator.iterate((1, 1)){case (x, y) => next(x, y)}): Option[(Int, Int)] =
      if (s.hasNext) {
        val e = exp.next()
        if (s.next() == e._1 /: e._2) check(s, exp)
        else Some(e)
      } else None


    val start = System.nanoTime()
    val set = rec(7, TreeSet.empty[Int], TreeSet((1 /: 1).l))
    val end = System.nanoTime()
    println(s"${set.size} elements generated in ${(end-start)/1000000}ms")

    check(set.iterator).take(1).map{case (n, d) => s"$n/$d"}.foreach(println)
  }

  def gcd(x: Int, y: Int): Int = {

    @tailrec
    def rec(a: Int, b: Int): Int = a % b match {
      case 0 => b
      case rem => rec(b, rem)
    }

    rec(x, y)
  }

  implicit class Rational(val l: Int) extends AnyVal {
    def numer: Int = 0x0000FFFF & l
    def denom: Int = 0x0000FFFF & (l >> 16)

    def /:(n: Int): Int = {
      val g = gcd(n, l)
      ((l/g) << 16) + n/g
    }

    def rString: String = s"$numer/$denom"

    def eq(r: Rational): Boolean = this.l == r.l
  }
}
