import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Utils.java2js

class MethodSpec extends AnyFlatSpec with Matchers {

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
    export default class Test {
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
    export default class Test {
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
    export default class Test {
      add() {
        if (arguments.length === 0) {
          return null;
        } else if (arguments.length === 1) {
          let x = arguments[0];
          return x;
        } else if (arguments.length === 2) {
          let x = arguments[0], y = arguments[1];
          return x + y;
        }
      }
    }
"""

    java2js(java) should equal (expected)
  }
  
}