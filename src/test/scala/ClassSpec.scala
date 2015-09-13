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
    import Double from 'java/lang/Double';
    export default class Test {
      constructor(...args) {
        this.init_(...args);
      }
      init_() {}
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
    import Double from 'java/lang/Double';
    export default class Test extends Object {
      constructor(...args) {
        super(...args);
        this.init_(...args);
      }
      init_() {}
    }

"""

    java2js(java) should equal (expected)
  }
}