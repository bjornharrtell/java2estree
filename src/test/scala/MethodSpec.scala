import org.scalatest.FlatSpec
import org.scalatest.Matchers

import Utils.java2js

class MethodSpec extends FlatSpec with Matchers {

  import Utils._
  
  "A Class with a method" should "translate into a Class with a method" in {
    val java =
"""
    class Test {
      void add() {
      }
    }
"""
    
    val expected =
"""
    export default class Test {
      get _interfaces() {
        return [];
      }
      add() {}
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
  
  "Method parameters" should "translate into a method parameters" in {
    val java =
"""
    class Test {
      void add(int x, int y) {
      }
    }
"""
    
    val expected =
"""
    export default class Test {
      get _interfaces() {
        return [];
      }
      add(x, y) {}
      getClass() {
        return Test;
      }
    }

"""
     java2js(java) should equal (expected)
  }
  
  
  "Overloaded methods" should "translate into a single method with overload logic" in {
    val java =
"""
    class Test {
      void add() {
        return null;
      }
      void add(int x) {
        return x;
      }
      void add(int x, int y) {
        return x+y;
      }
    }
"""
    
    val expected =
"""
    export default class Test {
      get _interfaces() {
        return [];
      }
      add(...args) {
        const overloads = (...args) => {
          switch (args.length) {
            case 0:
              return ((...args) => {
                let [] = args;
                return null;
              })(...args);
            case 1:
              return ((...args) => {
                let [x] = args;
                return x;
              })(...args);
            case 2:
              return ((...args) => {
                let [x, y] = args;
                return x + y;
              })(...args);
          }
        };
        return overloads.apply(this, args);
      }
      getClass() {
        return Test;
      }
    }

"""

    java2js(java) should equal (expected)
  }
  
}