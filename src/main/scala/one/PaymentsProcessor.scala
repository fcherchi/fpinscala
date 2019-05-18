package one

import org.slf4j.LoggerFactory

class PaymentsProcessor {
  val logger = LoggerFactory.getLogger(getClass.getName)
  def charge(creditCard: CreditCard, price: Double): Coffee = {
    logger.debug("Credit Card charged with {}", price)
    new Coffee()
  }

}
