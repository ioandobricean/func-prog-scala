package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.8/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s7 = singletonSet(7)
    val s1000 = singletonSet(1000)
    val negativeS3 = singletonSet(-3)
    
    val union12 = union(s1, s2)
    val union123 = union(union(s1, s2), s3)
    
    def pozitiveInt = (x: Int) => x > 0
    def negativeInt = (x: Int) => x < 0
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton s1")
      assert(contains(s2, 2), "Singleton s2")
      assert(!contains(s2, 3), "singleton s2not contain 3")
      assert(contains(negativeS3, -3), "singleton negativeSet3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      assert(contains(union12, 1), "Union 1")
      assert(contains(union12, 2), "Union 2")
      assert(!contains(union12, 3), "Union 3")
    }
  }

  test("intersec singleton sets") {
    new TestSets {
      val emptySet = intersect(s1, s2)
      assert(!contains(emptySet, 1), "empty set not contains 1")
      assert(!contains(emptySet, 2), "empty set not contains 2")
    }
  }

  test("intersec with one elem") {
    new TestSets {
      val union23 = union(s2, s3)
      val set2 = intersect(union12, union23)
      assert(!contains(set2, 1), "intersect not contains 1")
      assert(contains(set2, 2), "intersect contains 2")
      assert(!contains(set2, 3), "intersect not contains 3")
    }
  }

  test("intersec with 2 elem") {
    new TestSets {
      val set12 = intersect(union12, union12)
      assert(contains(set12, 1), "intersect contains 1")
      assert(contains(set12, 2), "intersect contains 2")
      assert(!contains(set12, 3), "intersect not contains 3")
    }
  }

  test("diff of 2 sets") {
    new TestSets {
      val s1Diff = diff(s1, s2)
      assert(contains(s1Diff, 1), "diff contains 1")
      assert(!contains(s1Diff, 2), "diff not contains 2")
    }
  }
  
  test("diff of 2 identical sets") {
    new TestSets {
      val emptyDiff = diff(s1, s1)
      assert(!contains(emptyDiff, 1), "diff not contains 1")
    }
  }
  
  test("diff of sets with common elem") {
    new TestSets {
      val s2Diff = diff(union12, s1)
      assert(!contains(s2Diff, 1), "diff not contains 1")
      assert(contains(s2Diff, 2), "diff contains 2")
    }
  }
  
  test("filter") {
    new TestSets {
      val filetedSet = filter(union123, ((x: Int) => x > 0))
      
      assert(contains(filetedSet, 1), "filtered set contains 1")
      assert(contains(filetedSet, 2), "filtered set contains 2")
      assert(!contains(filetedSet, -3), "filtered set not contains -3")
    }
  }
  
  test("forall") {
    new TestSets {
      val allSetIsPositive = forall(union123, ((x: Int) => x > 0))
      assert(allSetIsPositive, "set {1,2,3} is not all positive")
      
      val allSetIsNegative = forall(union123, ((x: Int) => x < 0))
      assert(!allSetIsNegative, "set {1,2,3} is all negative")
      
      val union12minus3 = union(union(s1, s2), negativeS3)
      val allPositive = forall(union12minus3, ((x: Int) => x > 0))
      assert(!allPositive, "set {1,2,-3} is not all positive")
    }
  }
  test("forall: {1,3,4,5,7,1000}") {
    new TestSets {
      val set = union(union(union(union(s1, s3), singletonSet(4)), singletonSet(5)), singletonSet(1000))
      val allSetIsPositive = forall(set, ((x: Int) => x < 5))
      assert(!allSetIsPositive, "All elements in the set are not strictly less than 5")
    }
  }
  
//  test("forall: {1,3,4,5,7,1000}") {
//    new TestSets {
//      val set = union(union(union(union(s1, s3), singletonSet(4)), singletonSet(5)), singletonSet(1000))
//      val allSetIsPositive = forall(set, ((x: Int) => x % 2 == 0))
//      assert(!allSetIsPositive, "All elements in the set are not strictly less than 5")
//    }
//  }
  
  
  test("exists") {
    new TestSets {
      val existsPositive = exists(union123, pozitiveInt)
      assert(existsPositive, "set union123 have positive number")
      
//      printSet(filter(union123, negativeInt))
      val existsNegative = exists(union123, negativeInt)
      assert(!existsNegative, "union123 doens't have negative number")
      
      val union12minus3 = union(union(s1, s2), negativeS3)
      printSet(filter(union12minus3, negativeInt))
      val existsNegative2 = exists(union12minus3, negativeInt)
      assert(existsNegative2, "set union12minus3 have negative number")
    }
  }
  
  test("map") {
    new TestSets {
      val negativeSet = map(union123, ((x: Int) => -x))
      
      assert(contains(negativeSet, -1), "inverted set not contains -1")
      assert(contains(negativeSet, -2), "inverted set not contains -2")
      assert(contains(negativeSet, -3), "inverted set not contains -3")
    }
  }
  test("map: {1,3,4,5,7,1000}") {
    new TestSets {
      val set = union(union(union(union(s1, s3), union(s4, s5)), s7), s1000)
      val negativeSet = map(set, ((x: Int) => x - 1))
      printSet(negativeSet)
      
      assert(contains(negativeSet, 0), "")
      assert(contains(negativeSet, 2), "")
      assert(contains(negativeSet, 4), "")
      assert(contains(negativeSet, 6), "")
      assert(contains(negativeSet, 999), "")
    }
  }
  
   test("map: {1,3,4,5,7,1000} doubling") {
    new TestSets {
      val set = union(union(union(union(union(s1, s3), singletonSet(4)), singletonSet(5)), singletonSet(7)), singletonSet(1000))
      val doublingSet = map(set, (x: Int) => x * 2)
      printSet(set)
      printSet(doublingSet)
      
      assert(contains(doublingSet, 2), "2")
      assert(contains(doublingSet, 6), "6")
      assert(contains(doublingSet, 8), "8")
      assert(contains(doublingSet, 10), "10")
      assert(contains(doublingSet, 14), "14")
    }
  }
  
}
