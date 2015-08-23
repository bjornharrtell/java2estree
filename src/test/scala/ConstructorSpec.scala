import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class ConstructorSpec extends FlatSpec with Matchers {

  import Utils._
  
  "A Class constructor " should "translate into a Class constructor" in {
    val java =
"""
    class Test {
      Test() {
      }
    }
"""
    
    val expected =
"""
    class Test {
      constructor() {}
    }

"""

    java2js(java) should equal (expected)
  }
}