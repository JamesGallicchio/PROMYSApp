package io.github.jamesgallicchio.promys

object Q7 {
  def main(args: Array[String]): Unit = {
    for (n <- 1 to 100) {
      print(s"\n$n: ")

      println((n+1 to n*(n+1)).count(a => (a * n) % (a - n) == 0))
    }
  }
}
