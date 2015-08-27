import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class MethodSpec extends FlatSpec with Matchers {

  import Utils._
  
  "A Class with a method" should "translate into a Class with a method" in {
    val java =
"""
    class Test {
      void add() {
      }
    }
"""
    
    val expected =
"""
    class Test {
      add() {}
    }

"""

    java2js(java) should equal (expected)
  }
  
  "Method parameters" should "translate into a method parameters" in {
    val java =
"""
    class Test {
      void add(int x, int y) {
      }
    }
"""
    
    val expected =
"""
    class Test {
      add(x, y) {}
    }

"""
     java2js(java) should equal (expected)
  }
  
  
  "Overloaded methods" should "translate into a single method with overload logic" in {
    val java =
"""
    class Test {
      void add() {
        return null;
      }
      void add(int x) {
        return x;
      }
      void add(int x, int y) {
        return x+y;
      }
    }
"""
    
    val expected =
"""
    class Test {
      add(...args) {
        switch (args.length) {
          case 2:
            return (function (x, y) {
              return x + y;
            })(...args);
            break;
          case 1:
            return (function (x) {
              return x;
            })(...args);
            break;
          case 0:
            return (function () {
              return null;
            })(...args);
            break;
        }
      }
    }

"""

    java2js(java) should equal (expected)
  }
  
}