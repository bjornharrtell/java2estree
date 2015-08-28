import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class ConstructorSpec extends FlatSpec with Matchers {

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
    class Test {
      constructor() {}
    }

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
    class Test {
      constructor(x, y) {}
    }

"""

    java2js(java) should equal (expected)
  }
  
  "Constructor member access" should "translate into member access" in {
    val java =
"""
    class Test {
      Test(int x) {
        this.x = x;
      }
    }
"""
    
    val expected =
"""
    class Test {
      constructor(x) {
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