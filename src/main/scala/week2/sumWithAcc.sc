object examples {
  def sum(f: Int => Int)(a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int)(a: Int, b: Int)Int
  
  sum(x => x)(1, 3)                               //> res0: Int = 6
  sum(x => x * x)(3, 5)                           //> res1: Int = 50
}