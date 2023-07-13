import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Utils.java2js

class ClassSpec extends AnyFlatSpec with Matchers {

  import Utils._
  
  "A Class" should "translate into a Class" in {
    val java =
"""
    class Test {}
"""
    
    val expected =
"""
    export default class Test {}
"""

    java2js(java) should equal (expected)
  }
  
  "A Class with extends" should "translate into a Class with extends" in {
    val java =
"""
    class Test extends Object {}
"""
    
    val expected =
"""
    export default class Test extends Object {
      constructor() {
        super();
      }
    }
"""

    java2js(java) should equal (expected)
  }
}