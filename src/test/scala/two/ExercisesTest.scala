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
}
