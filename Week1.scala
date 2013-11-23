package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(list: List[Char], openCount: Int): Boolean = {
      if (openCount == -1) false
      else if (list.isEmpty && openCount == 0) true
      else if (list.isEmpty && openCount != 0) false
      else if (list.head == '(') balance(list.tail, openCount + 1)
      else if (list.head == ')') balance(list.tail, openCount - 1)
      else balance(list.tail, openCount)
    }
    balance(chars, 0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
