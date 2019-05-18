package one

import org.slf4j.LoggerFactory

object Main extends App {

  val logger = LoggerFactory.getLogger(getClass.getSimpleName)


//  val transaction = new Cafe().buyCoffee(new CreditCard)
//
//  logger.debug("The transaction is finished. You get one coffee:")
//  logger.debug("{}", transaction._1)
//  logger.debug("The transaction is finished. You pay:")
//  logger.debug("{}", transaction._2)


  // val order = new Cafe().buyNCoffeesLoop(new CreditCard, 3)
  val order = new Cafe().buyNCoffees(new CreditCard, 3)
  logger.debug("Order consist of {}", order._1)
  logger.debug("Total Price charged {}", order._2)

}
