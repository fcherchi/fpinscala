package two

import org.scalatest.FunSuite

class ExercisesTest extends FunSuite {

  test("Fibonacci function should return 5 if arg is 5" ) {
    assert(Exercises.fibonacci(5) == 5)
  }

  test("Fibonacci function should return 1 if arg is 1" ) {
    assert(Exercises.fibonacci(1) == 1)
  }

  test("Fibonacci function should return 13 if arg is 7" ) {
    assert(Exercises.fibonacci(7) == 13)
  }


  test("This array is sorted for ints" ) {
    assertResult(true) {Exercises.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b)}
  }

  test("This array is not sorted for ints" ) {
    assertResult(false) {Exercises.isSorted(Array(1, 3, 2), (a: Int, b: Int) => a < b)}
  }

  test("This array is sorted for strings" ) {
    assertResult(true) {Exercises.isSorted(Array("a", "b", "c"), (a: String, b: String) => a < b)}
  }

  test("This array is not sorted for strings" ) {
    assertResult(false) {Exercises.isSorted(Array("c", "b", "a"), (a: String, b: String) => a < b)}
  }
}
