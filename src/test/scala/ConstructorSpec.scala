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
    export default class Test {
      constructor(...args) {
        (() => {})();
        const overloads = (...args) => {
          switch (args.length) {
            case 0:
              return ((...args) => {
                let [] = args;
              })(...args);
          }
        };
        return overloads.apply(this, args);
      }
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
      constructor(...args) {
        (() => {})();
        const overloads = (...args) => {
          switch (args.length) {
            case 2:
              return ((...args) => {
                let [x, y] = args;
              })(...args);
          }
        };
        return overloads.apply(this, args);
      }
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
      constructor(...args) {
        (() => {
          this.x = null;
        })();
        const overloads = (...args) => {
          switch (args.length) {
            case 1:
              return ((...args) => {
                let [x] = args;
                this.x = x;
              })(...args);
          }
        };
        return overloads.apply(this, args);
      }
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