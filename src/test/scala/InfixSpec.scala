import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class InfixSpec extends FlatSpec with Matchers {

  import Utils._
  
  "An InfixExpression" should " with extended ops should translate cleanly" in {
    val java =
"""
    class Test {
      int calc() {
        return 1 + 2 + 3;
      }
    }

"""
    
    val expected =
"""
    export default class Test {
      get _interfaces() {
        return [];
      }
      calc() {
        return 1 + 2 + 3;
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
  
  "An InfixExpression" should " with ints and division should be truncated" in {
    val java =
"""
    class Test {
      int calc() {
        return 1 / 2;
      }
    }

"""

    val expected =
"""
    export default class Test {
      get _interfaces() {
        return [];
      }
      calc() {
        return Math.trunc(1 / 2);
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
  
  "An InfixExpression" should " with ints and division and exops should be truncated" in {
    val java =
"""
    class Test {
      int calc() {
        return 1 / 2 / 2;
      }
    }

"""

    val expected =
"""
    export default class Test {
      get _interfaces() {
        return [];
      }
      calc() {
        return Math.trunc(Math.trunc(1 / 2) / 2);
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
}