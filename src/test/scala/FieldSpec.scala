import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Utils.java2js

class FieldSpec extends AnyFlatSpec with Matchers {

  import Utils._
  
  /*
  "A field" should "translate into a set get with hidden member" in {
    val java =
"""
    class Test {
      int i = 0;
      Test() {
      }
    }
"""
    
    val expected =
"""
    class Test {
      get i() {
        return this.i_;
      }
      set i(val) {
        this.i_ = val;
      }
      constructor() {
        this.i_ = 0;
      }
    }

"""

    java2js(java) should equal (expected)
  }
  */
}