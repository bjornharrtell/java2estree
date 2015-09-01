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
    export default class Test {
      constructor(...args) {
        this.init_(...args);
      }
      init_() {}
    }

"""

    java2js(java) should equal (expected)
  }
}