package one

class Cafe {


  /**
    * First example with side effect. Not functional
    * @param creditCard here is where the side effect takes place
    * @return a Coffee object
    */
  def buyCoffeeOne(creditCard: CreditCard): Coffee = {
    val cup = new Coffee()

    //side effect into credit card. how about calls to credit card issuer, etc._
    creditCard.charge(cup.price)

    //last line is always a return sentence
    cup
  }

  /**
    * Another suboptimal implementation. If you want to order 3 coffees you call 3 times to the card issuer
    * @param creditCard
    * @param paymentsProcessor
    * @return
    */
  def buyCoffeeTwo(creditCard: CreditCard, paymentsProcessor: PaymentsProcessor): Coffee ={
    val cup = new Coffee()

    //still a side effect but more testable (easy to mock paymentsProcessor)
    paymentsProcessor.charge(creditCard, cup.price)
  }


  /**
    * Now it is functional. No side effect. Returns a pair of values.
    * @param creditCard
    * @return
    */
  def buyCoffee(creditCard: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()

    //returning a pair. A cup of coffee and a Charge object.
    (cup, Charge(creditCard, cup.price))
  }


  /**
    * Example with a loop. Very verbose and cumbersome
    * @param creditCard
    * @param howMany
    * @return
    */
  def buyNCoffeesLoop(creditCard: CreditCard, howMany: Int): (List[Coffee], Charge) = {
    var cups: List[Coffee] = List()
    var charge: Charge = Charge(creditCard, 0)

    //loop, we don't need an index
    for (_ <- 1 to howMany) {
      val pair = buyCoffee(creditCard)
      cups = cups.::(pair._1)
      charge = charge.combine(pair._2)
    }

    //returning the whole order
    (cups, charge)
  }


  /**
    * Functional version
    * @param creditCard
    * @param howMany
    * @return
    */
  def buyNCoffees(creditCard: CreditCard, howMany: Int): (List[Coffee], Charge) = {

    //create the order in one go, using list fill method, first param is n, second is function to create 1 element
    val order: List[(Coffee, Charge)] = List.fill(howMany)(buyCoffee(creditCard))

    //unzip creates a pair of lists out of a list of pairs
    val lists = order.unzip

    //this is a list of coffee
    val coffees: List[Coffee] = lists._1
    //this one is a list of payments
    val transactions: List[Charge] = lists._2

    //now we want to reduce (combine) all transactions in one
    val charge = transactions.reduce((x, y) => x.combine(y))



    //all in one line
    //this one does not compile
    //val (a, b) = order.unzip(a, b.reduce((c1, c2) => c1.combine(c2)))
    // this one works
    //(lists._1, lists._2.reduce((a, b) => a.combine(b)))

    //returning the pair
    (coffees, charge)
  }

}
