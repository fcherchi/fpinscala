import two.PolymorphicFunctions

import scala.annotation.tailrec
//package three

//import org.slf4j.LoggerFactory

//sealed means all implementations of the trait must be in this class
//+A is covariant
sealed trait Lista[+A]

//nothing is a subtype of all types in Scala
case object Nil extends Lista[Nothing]

case class NonEmpty[+A](head: A, tail: Lista[A]) extends Lista[A]

//companion object
object Lista {

  //val logger = LoggerFactory.getLogger(getClass.getSimpleName)

  //adds up all elements in a list of integer
  def sum(ints: Lista[Int]): Int = {
    //pattern matching
    ints match {
      //stop iteration if list is nil (last element or empty)
      case Nil => 0
      // if is not null, iterate from head to bottom
      case NonEmpty(head, tail) => head + sum(tail)
    }
  }

  def product(ints: Lista[Double]): Double = {
    //pattern matching
    ints match {
      case Nil => 1.0

      // if head is 0, the result is 0
      case NonEmpty(0.0, _) => 0.0

      // if it is not null, iterate from head to bottom
      case NonEmpty(head, tail) => head * product(tail)
    }
  }

  //A* means zero or more args of type A (variadic)
  def apply[A](args: A*): Lista[A] = {
    //this methods from A* are from the type Seq
    if (args.isEmpty) Nil
    else NonEmpty(args.head, apply(args.tail: _*))
    //_* is the way to pass a param to an A* type
  }

  /**
    * Removes first element (head)
    *
    * @param lista
    * @tparam A
    * @return
    */
  def tail[A](lista: Lista[A]): Lista[A] = {
    lista match {
      case Nil => Nil
      case NonEmpty(_, tail) => tail
    }
  }

  /**
    * Changes the first element for the one received
    *
    * @param lista
    * @tparam A
    * @return
    */
  def setHead[A](newHead: A, lista: Lista[A]): Lista[A] = {
    lista match {
      case Nil => NonEmpty(newHead, Nil)
      case NonEmpty(_, tail) => NonEmpty(newHead, tail)
    }
  }

  @tailrec
  def drop[A](howMany: Int, lista: Lista[A]): Lista[A] = {
    lista match {
      case Nil => Nil
      case NonEmpty(_, tail) =>
        if (howMany > 1) drop(howMany - 1, tail)
        else if (howMany < 1) lista
        else tail
    }
  }

  /**
    * Stops removing in the first false
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  @tailrec
  def dropWhile[A](l: Lista[A], f: A => Boolean): Lista[A] =
    l match {


      //Pattern matching accepts guard clauses or extra conditions
      case NonEmpty(head,tail) if !f(head) => dropWhile(tail, f)
        //fallback
      case _ => l
    }

  // this last dropWhile has to be called like this:
  //  Lista.dropWhile(Lista(1,2,3,4,5,6), (x:Int) => x % 2 == 0)

  //
  // with this one we dont need the cast because it returns a function that then returns the list

  @tailrec
  def dropWhileCurried[A](l: Lista[A]) (f: A => Boolean): Lista[A] =
    l match {
      case NonEmpty(head,tail) if !f(head) => dropWhileCurried(tail)(f)
      case _ => l
    }


  /**
    * Non tail rec, not so nice, but an interesting mind game
    * @param l1
    * @param l2
    * @tparam A
    * @return
    */
  def append[A](l1: Lista[A], l2: Lista[A]): Lista[A] = {
    l1 match {
      case Nil => l2 //exit condition
      case NonEmpty(head, tail) => NonEmpty(head, append(tail, l2))
    }
  }

  //nasty case, remove last element

//  def removeLast[A](l1: Lista[A]): Lista[A] = {
//    l1 match {
//      case Nil => Nil
//      case NonEmpty(_, Nil) => Nil
//      case NonEmpty(head, tail) => {
//
//
//        def loop(l: Lista[A]): Lista[A] = {
//
//        }
//      }
//    }
//  }


  def main(args: Array[String]): Unit = {
   // val l: Lista[Double] = NonEmpty(2.0, Nil)


    //pattern matching crazy example

    val x = Lista(1,2,3,4,5) match {
      case NonEmpty(x, NonEmpty(2, NonEmpty(4, _))) => x
      case Nil => 42
      case NonEmpty(x, NonEmpty(y, NonEmpty(3, NonEmpty(4, _)))) => x + y
      case NonEmpty(h, t) => h + Lista.sum(t)
      case _ => 101
    }


    //val l = Lista.dropWhileCurried(Lista(1,2,3,4,5,6))(x => x % 2 == 0)

   // val dropWhileCurried = PolymorphicFunctions.curry(dropWhile(_: Lista[Int], _: Int => Boolean))

    System.out.println(dropWhile(Lista(1,2,3,4,5,6), (x:Int) => x % 2 == 0))
    System.out.println(dropWhileCurried(Lista(1,2,3,4,5,6))(x => x % 2 == 0))

   // logger.debug("" + Lista.product(l))

    //logger.debug("" + Lista.product(l))
  }
}
