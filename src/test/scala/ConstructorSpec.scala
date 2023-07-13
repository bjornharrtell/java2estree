import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Utils.java2js

class ConstructorSpec extends AnyFlatSpec with Matchers {

  import Utils._
  
  "A Class constructor" should "translate into a Class constructor" in {
    val java =
"""
    class Test {
      Test() {
      }
    }
"""
    
    val expected =
"""
    export default class Test {}
"""

    java2js(java) should equal (expected)
  }
  
  "Constructor parameters" should "translate into a constructor parameters" in {
    val java =
"""
    class Test {
      Test(int x, int y) {
      }
    }
"""
    
    val expected =
"""
    export default class Test {
      constructor() {
        Test.constructor_.apply(this, arguments);
      }
      static constructor_() {
        let x = arguments[0], y = arguments[1];
      }
    }
"""

    java2js(java) should equal (expected)
  }
  
  "Constructor member access" should "translate into member access" in {
    val java =
"""
    class Test {
      int x;
      Test(int x) {
        this.x = x;
      }
    }
"""
    
    val expected =
"""
    export default class Test {
      constructor() {
        Test.constructor_.apply(this, arguments);
      }
      static constructor_() {
        this.x = null;
        let x = arguments[0];
        this.x = x;
      }
    }
"""

    java2js(java) should equal (expected)
  }
  
  /*
  "Constructor overloading " should "translate into ..." in {
    val java =
"""
    class Test {
      Test() {
        
      }
      Test(int x) {
        this();
      }
    }
"""
    
    val expected =
"""
    class Test {
      constructor(x, y) {
      }
    }

"""

    java2js(java) should equal (expected)
  }
  */
}