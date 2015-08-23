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
}