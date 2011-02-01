package com.zilverline.examples.immutabledomain.jpa

import org.specs.Specification
import JpaSupport._
import javax.persistence.PersistenceException
import java.math.BigDecimal
import org.joda.time.{DateTimeUtils, LocalDate}

class JpaInvoiceSpec extends Specification {

  implicit val emf = JpaSupport.entityManagerFactory
  implicit def string2BigDecimal(amount: String) = new BigDecimal(amount)

  doBeforeSpec {
    DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 1, 29).toDateTimeAtCurrentTime.getMillis)
  }
  doAfterSpec {
    DateTimeUtils.setCurrentMillisSystem
  }

  "draft invoice" should {
    val invoice = new Invoice
    "be persisted" in {
      invoice.recipient must beEqualTo(None)
      invoice.recipient_? must beFalse
      invoice.items_? must beFalse

      withEntityManager(_.persist(invoice)) mustNot throwA[PersistenceException]
    }

    "become ready to send when recipient and at least one item are specified" in {
      invoice.recipient = Some("Erik")
      invoice.addItem("Programming Scala", "42.95")

      invoice.recipient_? must beTrue
      invoice.items_? must beTrue
      invoice.readyToSend_? must beTrue
      invoice.sentDate must beNull
      invoice.dueDate must beNull
      invoice.totalAmount must beEqualTo("42.95": BigDecimal)
      withEntityManager(_.persist(invoice)) mustNot throwA[PersistenceException]

      invoice.removeItem(0)
      invoice.readyToSend_? must beFalse
      withEntityManager(_.merge(invoice)) mustNot throwA[PersistenceException]
    }
  }

  "sent invoice" should {
    val invoice = new Invoice
    invoice.recipient = Some("Erik")
    invoice.addItem("Programming Scala", "42.95")

    invoice.send

    "have sent and due date" in {
      invoice.sentDate must beEqualTo(new LocalDate(2011, 1, 29))
      invoice.dueDate must beEqualTo(new LocalDate(2011, 2, 12))
      invoice.readyToSend_? must beFalse
    }

    "be marked as paid when paid" in {
      invoice.paymentReceived

      invoice.paid_? must beTrue
      invoice.paymentDate must beEqualTo(new LocalDate(2011, 1, 29))

      withEntityManager(_.persist(invoice)) mustNot throwA[PersistenceException]
    }

    "be late when dueDate is in the past" in {
      invoice.late_? must beFalse
      DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 2, 20).toDateTimeAtStartOfDay.getMillis)
      invoice.late_? must beTrue

      invoice.remind

      invoice.reminderDate must beEqualTo(new LocalDate(2011, 2, 20))
      withEntityManager(_.persist(invoice)) mustNot throwA[PersistenceException]
    }
  }
}
