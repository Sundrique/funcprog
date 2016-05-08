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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balancedOpen(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balancedOpen(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && balancedOpen(chars.tail, open - 1)
      else balancedOpen(chars.tail, open)
    }
    balancedOpen(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int], acc: Int): Int = {
      if (coins.isEmpty) acc
      else loop(money, coins.tail, acc + countChange(money - coins.head, coins))
    }
    if (money == 0) 1
    else if (coins.isEmpty || money < 0)  0
    else loop(money, coins, 0)
  }
}
