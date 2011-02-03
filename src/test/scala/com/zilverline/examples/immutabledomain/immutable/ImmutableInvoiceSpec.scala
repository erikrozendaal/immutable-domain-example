package com.zilverline.examples.immutabledomain.immutable

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

  "new invoice" should {
    val invoice = new Invoice(1)
    "be created" in {
      invoice.uncommittedEvents must contain(InvoiceCreated(1))
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

  "invoice report" should {
    "track invoice changes" in {
      val subject = InvoiceReport
        .handleEvent(InvoiceCreated(17))
        .handleEvent(InvoiceRecipientChanged(17, Some("Erik")))
        .handleEvent(InvoiceItemAdded(17, InvoiceItem(1, "Food", 2.95), 2.95))
        .handleEvent(InvoiceSent(17, new LocalDate(2011, 1, 5), new LocalDate(2011, 1, 19)))
        .handleEvent(InvoiceReminderSent(17, new LocalDate(2011, 1, 21)))
        .handleEvent(InvoicePaymentReceived(17, new LocalDate(2011, 1, 25)))

      subject.id must beEqualTo(17)
      subject.recipient must beEqualTo(Some("Erik"))
      subject.items.values must contain(InvoiceItem(1, "Food", 2.95))
      subject.sentDate must beEqualTo(Some(new LocalDate(2011, 1, 5)))
      subject.dueDate must beEqualTo(Some(new LocalDate(2011, 1, 19)))
      subject.reminderDates must beEqualTo(List(new LocalDate(2011, 1, 21)))
      subject.paymentDate must beEqualTo(Some(new LocalDate(2011, 1, 25)))
    }
  }

}