import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class ClassSpec extends FlatSpec with Matchers {

  import Utils._
  
  "A Class " should "translate into a Class" in {
    val java =
"""
    class Test {
      void add(int x, int y) {
        return x + y;
      }
    }

"""
    
    val expected =
"""
    class Test {
      add(x, y) {
        return x + y;
      }
    }

"""

    java2js(java) should equal (expected)
  }
}