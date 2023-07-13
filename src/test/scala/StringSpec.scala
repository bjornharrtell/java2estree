import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Utils.java2js

class StringSpec extends AnyFlatSpec with Matchers {

  import Utils._
  
  "String length method calls" should "translate into length property access" in {
    val java =
"""
    class Test {
      public void test(String a) {
        int l = a.length();
        String b = "";
        l = b.length();
        l = "".length();
      }
    }

"""
    
    val expected =
"""
    export default class Test {
      get interfaces_() {
        return [];
      }
      test(a) {
        var l = a.length;
        var b = "";
        l = b.length;
        l = "".length;
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
}