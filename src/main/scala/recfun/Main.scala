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
   * Exercise 1 : Pascal Triangle
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0) 0 // the index is out of triangle 	  
    else if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(list: List[Char], parCount: Int): Boolean = {
      if (list.isEmpty) parCount == 0
      else if (list.head == '(')
        isBalanced(list.tail, parCount + 1)
      else if (list.head == ')')
        if (parCount == 0) false
        else isBalanced(list.tail, parCount - 1)
      else
        isBalanced(list.tail, parCount)
    }

    if (chars.isEmpty) true
    else isBalanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {    
    def countChangeOfMoney(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money == coins.head) 1
      else if (money > coins.head)
        countChangeOfMoney(money - coins.head, coins) + countChangeOfMoney(money, coins.tail)
      else 0
    }
    
    if (money == 0 || coins.isEmpty) 0
    else  countChangeOfMoney(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
