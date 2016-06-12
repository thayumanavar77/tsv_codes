package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (r == 0 || c == 0 || r == c) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)

  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def testBalancedParen(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty)
          count == 0
        else if (chars.head == '(')
          testBalancedParen(chars.tail, count + 1)
        else if (chars.head == ')' )
           if (count > 0 )
             testBalancedParen(chars.tail, count - 1)
           else
              false
        else
          testBalancedParen(chars.tail, count)
      }
      testBalancedParen(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
      if (money == 0) 1
      else if (coins.isEmpty)  0
      else if (money > 0)
        countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
      else 0

}