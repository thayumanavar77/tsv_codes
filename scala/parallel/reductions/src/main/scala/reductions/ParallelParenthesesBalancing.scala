package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */


  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceAcc(index: Int, balanceCount: Int): Boolean = {
      if ( balanceCount < 0) false
      else if ( index == chars.length) balanceCount == 0
      else if (chars(index) == '(') balanceAcc(index + 1, balanceCount + 1)
      else if (chars(index) == ')') balanceAcc(index + 1, balanceCount - 1)
      else balanceAcc(index + 1, balanceCount)
    }
    balanceAcc(0,0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      var (tparen, rparen) = (0,0)
      for (index <- idx until until)
      {
        if (chars(index) == '(') tparen = tparen + 1

        if (chars(index) == ')') tparen = tparen - 1
        if (tparen < rparen) rparen = tparen
      }
      (tparen, rparen)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (from > until) (0,0)
      if ( (until - from) <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = from + (until - from) / 2
        val (x, y) = parallel(reduce(from, middle),
                              reduce(middle, until))
        (x._1 + y._1,Math.min(x._2, x._1 + y._2))
      }

    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
