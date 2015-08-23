import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class ClassSpec extends FlatSpec with Matchers {

  import Utils._
  
  "A Class" should "translate into a Class" in {
    val java =
"""
    class Test {
    }

"""
    
    val expected =
"""
    class Test {}

"""

    java2js(java) should equal (expected)
  }
}