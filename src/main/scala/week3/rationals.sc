object rationals {
  val x = new Rational(1, 2)                      //> x  : Rational = 1/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 2

  x.add(new Rational(2, 3))                       //> res2: Rational = 7/6

  val n1 = new Rational(1, 4)                     //> n1  : Rational = 1/4
  val n2 = new Rational(5, 7)                     //> n2  : Rational = 5/7
  val n3 = new Rational(3, 2)                     //> n3  : Rational = 3/2
  n1.sub(n2).sub(n3)                              //> res3: Rational = -55/28
  n1.sub(new Rational(1, 4))                      //> res4: Rational = 0/1

  n1.less(n2)                                     //> res5: Boolean = true
  n1.max(n2)                                      //> res6: Rational = 5/7
  
  new Rational(2)                                 //> res7: Rational = 2/1
}

class Rational(x: Int, y: Int) {
	require(y != 0, "denominator must be nonzero")

	def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  val numer = x / gcd(x, y)
  val denom = y / gcd(x, y)

  def less(that: Rational) = numer * that.denom < that.numer * denom

	def max(that: Rational) = if (this.less(that)) that else this

  def add(that: Rational): Rational =
    new Rational(
      that.numer * denom + numer * that.denom,
      that.denom * denom)

  def neg: Rational = new Rational(-numer, denom)

  def sub(s: Rational): Rational = add(s.neg)

  override def toString = numer + "/" + denom
}