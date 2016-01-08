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
      get interfaces_() {
        return [];
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
  
  "A Class with extends" should "translate into a Class with extends" in {
    val java =
"""
    class Test extends Object {
    }

"""
    
    val expected =
"""
    export default class Test extends Object {
      get interfaces_() {
        return [];
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
}