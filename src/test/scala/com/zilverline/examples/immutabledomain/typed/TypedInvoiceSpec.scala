package com.zilverline.examples.immutabledomain.typed

import org.specs.Specification
import org.joda.time.{DateTimeUtils, LocalDate}
import org.specs.runner.JUnit4
import com.zilverline.examples.immutabledomain.events._

class TypedInvoiceTest extends JUnit4(TypedInvoiceSpec)

object TypedInvoiceSpec extends Specification {

  doBeforeSpec {
    DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 1, 29).toDateTimeAtCurrentTime.getMillis)
  }
  doAfterSpec {
    DateTimeUtils.setCurrentMillisSystem
  }

  implicit def string2BigDecimal(amount: String) = BigDecimal(amount)

  "invoice loaded from history" should {
    val invoice = Invoice.loadFromHistory[Invoice](Seq(
      InvoiceCreated(2),
      InvoiceRecipientChanged(2, Some("Erik"))))

    "have no uncommitted events" in {
      invoice.uncommittedEvents must beEmpty
    }
  }

  "new invoice" should {
    val invoice = Invoice.create(1)
    "be created" in {
      invoice.uncommittedEvents must contain(InvoiceCreated(1))
    }

    "clear uncommitted events" in {
      invoice.markCommitted
        .uncommittedEvents must beEmpty
    }
  }

  "ready to send invoice" should {
    "generate invoice sent event" in {
      val invoice = Invoice.loadFromHistory[DraftInvoice](Seq(
        InvoiceCreated(1),
        InvoiceRecipientChanged(1, Some("Erik")),
        InvoiceItemAdded(1, InvoiceItem(1, "Food", 2.95), 2.95)))

      invoice.send.uncommittedEvents must contain(
        InvoiceSent(1,
          sentDate = new LocalDate(2011, 1, 29),
          dueDate = new LocalDate(2011, 2, 12)))
    }
  }

  "draft invoice" should {
    val invoice: DraftInvoice = Invoice.loadFromHistory(Seq(InvoiceCreated(1)))

    "support changing the recipient" in {
      invoice.changeRecipient(Some("Erik"))
        .uncommittedEvents must contain(InvoiceRecipientChanged(1, Some("Erik")))
    }

    "support adding invoice items" in {
      val updated = invoice
        .addItem("Food", "2.95")
        .addItem("Water", "1.95")
        .removeItem(1)

      updated.uncommittedEvents must contain(InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95"))
      updated.uncommittedEvents must contain(InvoiceItemAdded(1, InvoiceItem(2, "Water", "1.95"), "4.90"))
      updated.uncommittedEvents must contain(InvoiceItemRemoved(1, InvoiceItem(1, "Food", "2.95"), "1.95"))
    }

    "not be ready to send" in {
      invoice.readyToSend_? must beFalse
      invoice.send must throwA[IllegalArgumentException]
    }
  }

  "fully specified draft invoice" should {
    val invoice = Invoice.loadFromHistory[DraftInvoice](Seq(
      InvoiceCreated(1),
      InvoiceRecipientChanged(1, Some("Erik")),
      InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95")))

    "be ready to send" in {
      invoice.readyToSend_? must beTrue

      invoice.send.uncommittedEvents must contain(InvoiceSent(1,
        sentDate = new LocalDate(2011, 1, 29),
        dueDate = new LocalDate(2011, 2, 12)))
    }
  }

  "sent invoice" should {
    val invoice = Invoice.loadFromHistory[SentInvoice](Seq(
      InvoiceCreated(1),
      InvoiceRecipientChanged(1, Some("Erik")),
      InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95"),
      InvoiceSent(1, new LocalDate(2011, 1, 29), new LocalDate(2011, 2, 12))))

    "be marked as paid when paid" in {
      invoice.pay
        .uncommittedEvents must contain(InvoicePaymentReceived(1, new LocalDate(2011, 1, 29)))
    }

    "be late when dueDate is in the past" in {
      invoice.late_? must beFalse
      DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 2, 20).toDateTimeAtStartOfDay.getMillis)
      invoice.late_? must beTrue

      invoice.remind
        .uncommittedEvents must contain(InvoiceReminderSent(1, new LocalDate(2011, 2, 20)))
    }
  }

}