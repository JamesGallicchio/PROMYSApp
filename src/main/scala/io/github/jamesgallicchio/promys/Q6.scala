package io.github.jamesgallicchio.promys

object Q6 {
  def main(args: Array[String]): Unit = {
    (1000000L to 1000000000L).toStream.map{i =>
      val squared = i*i
      val s = squared.toString
      (i, squared, s)
    }.filter{case (_, _, s) =>
      val l = s.length
      s.substring(0, l/2).equals(s.substring(l/2))
    }.foreach(println)
  }
}
