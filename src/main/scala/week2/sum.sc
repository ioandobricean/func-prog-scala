object sum {
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }                                               //> sum: (f: Int => Int)(Int, Int) => Int

  def sumInts = sum(x => x)                       //> sumInts: => (Int, Int) => Int
  def sumCubes = sum(x => x * x * x)              //> sumCubes: => (Int, Int) => Int

  sumInts(1, 4)                                   //> res0: Int = 10
  sumCubes(2, 3)                                  //> res1: Int = 35

  def cube(x: Int): Int = x * x * x               //> cube: (x: Int)Int

  sum(cube)(2, 3)                                 //> res2: Int = 35

  def sum2(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum2(f)(a + 1, b)    //> sum2: (f: Int => Int)(a: Int, b: Int)Int
    
  sum2(cube)(2, 3)                                //> res3: Int = 35

}