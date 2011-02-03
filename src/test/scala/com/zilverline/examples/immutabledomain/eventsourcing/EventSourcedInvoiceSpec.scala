package com.zilverline.examples.immutabledomain.eventsourcing

import org.specs.Specification
import org.joda.time.{DateTimeUtils, LocalDate}
import org.specs.runner.JUnit4
import com.zilverline.examples.immutabledomain.events._

class EventSourcedInvoiceTest extends JUnit4(EventSourcedInvoiceSpec)

object EventSourcedInvoiceSpec extends Specification {

  doBeforeSpec {
    DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 1, 29).toDateTimeAtCurrentTime.getMillis)
  }
  doAfterSpec {
    DateTimeUtils.setCurrentMillisSystem
  }

  implicit def string2BigDecimal(amount: String) = BigDecimal(amount)

  "invoice loaded from history" should {
    val invoice = new Invoice
    invoice.loadFromHistory(Seq(
      InvoiceCreated(2),
      InvoiceRecipientChanged(2, Some("Erik"))))

    "have no uncommitted events" in {
      invoice.uncommittedEvents must beEmpty
    }
  }

  "new invoice" should {
    val invoice = new Invoice(1)
    "be created" in {
      invoice.uncommittedEvents must contain(InvoiceCreated(1))
    }

    "clear uncommitted events" in {
      invoice.markCommitted

      invoice.uncommittedEvents must beEmpty
    }
  }

  "ready to send invoice" should {
    "generate invoice sent event" in {
      val invoice = new Invoice
      invoice.loadFromHistory(Seq(
        InvoiceCreated(1),
        InvoiceRecipientChanged(1, Some("Erik")),
        InvoiceItemAdded(1, InvoiceItem(1, "Food", 2.95), 2.95)))

      invoice.send

      invoice.uncommittedEvents must contain(
        InvoiceSent(1,
          sentDate = new LocalDate(2011, 1, 29),
          dueDate = new LocalDate(2011, 2, 12)))
    }
  }

  "draft invoice" should {
    val invoice = new Invoice
    invoice.loadFromHistory(Seq(InvoiceCreated(1)))

    "support changing the recipient" in {
      invoice.changeRecipient(Some("Erik"))

      invoice.uncommittedEvents must contain(InvoiceRecipientChanged(1, Some("Erik")))
    }

    "support adding invoice items" in {
      invoice.addItem("Food", "2.95")
      invoice.addItem("Water", "1.95")
      invoice.removeItem(1)

      invoice.uncommittedEvents must contain(InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95"))
      invoice.uncommittedEvents must contain(InvoiceItemAdded(1, InvoiceItem(2, "Water", "1.95"), "4.90"))
      invoice.uncommittedEvents must contain(InvoiceItemRemoved(1, InvoiceItem(1, "Food", "2.95"), "1.95"))
    }

    "not be ready to send" in {
      invoice.readyToSend_? must beFalse
      invoice.send must throwA[IllegalArgumentException]
    }

    "not be payable" in {
      invoice.readyToPay_? must beFalse
      invoice.pay must throwA[IllegalArgumentException]
    }
  }

  "fully specified draft invoice" should {
    val invoice = new Invoice
    invoice.loadFromHistory(Seq(
      InvoiceCreated(1),
      InvoiceRecipientChanged(1, Some("Erik")),
      InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95")))

    "be ready to send" in {
      invoice.readyToSend_? must beTrue

      invoice.send

      invoice.uncommittedEvents must contain(InvoiceSent(1,
        sentDate = new LocalDate(2011, 1, 29),
        dueDate = new LocalDate(2011, 2, 12)))
    }

    "not be payable" in {
      invoice.readyToPay_? must beFalse
      invoice.pay must throwA[IllegalArgumentException]
    }
  }

  "sent invoice" should {
    val invoice = new Invoice
    invoice.loadFromHistory(Seq(
      InvoiceCreated(1),
      InvoiceRecipientChanged(1, Some("Erik")),
      InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95"),
      InvoiceSent(1, new LocalDate(2011, 1, 29), new LocalDate(2011, 2, 12))))

    "not be ready to send" in {
      invoice.readyToSend_? must beFalse
      invoice.send must throwA[IllegalArgumentException]
    }

    "be marked as paid when paid" in {
      invoice.readyToPay_? must beTrue
      invoice.pay

      invoice.uncommittedEvents must contain(InvoicePaymentReceived(1, new LocalDate(2011, 1, 29)))
    }

    "be late when dueDate is in the past" in {
      invoice.late_? must beFalse
      DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 2, 20).toDateTimeAtStartOfDay.getMillis)
      invoice.late_? must beTrue

      invoice.remind

      invoice.uncommittedEvents must contain(InvoiceReminderSent(1, new LocalDate(2011, 2, 20)))
    }
  }

}
