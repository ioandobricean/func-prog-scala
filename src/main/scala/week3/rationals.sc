object rationals {
  val x = new Rational(1, 2)                      //> x  : Rational = 1/2
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 2

	x.add(new Rational(2,3))                  //> res2: Rational = 7/6

	val n1 = new Rational(1,3)                //> n1  : Rational = 1/3
	n1.sub(new Rational(5,7)).sub(new Rational(3,2))
                                                  //> res3: Rational = -79/42
	n1.sub(new Rational(1,4))                 //> res4: Rational = 1/12
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y

  def add(that: Rational): Rational =
    new Rational(
      that.numer * denom + numer * that.denom,
      that.denom * denom)
      
  def neg: Rational = new Rational(-numer, denom)
  
  def sub(s: Rational): Rational = add(s.neg)
      
  override def toString = numer + "/" + denom
}