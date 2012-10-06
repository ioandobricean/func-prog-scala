object superclass {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  (new Sub).foo                                   //> res0: Int = 2
}

abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
  override def foo = 2
  def bar = 3
}