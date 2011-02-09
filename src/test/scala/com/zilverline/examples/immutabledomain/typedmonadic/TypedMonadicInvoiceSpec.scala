package com.zilverline.examples.immutabledomain.typedmonadic

import org.specs.Specification
import org.joda.time.{DateTimeUtils, LocalDate}
import org.specs.runner.JUnit4
import com.zilverline.examples.immutabledomain.events._

class TypedMonadicInvoiceTest extends JUnit4(TypedMonadicInvoiceSpec)

object TypedMonadicInvoiceSpec extends Specification {

  doBeforeSpec {
    DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 1, 29).toDateTimeAtCurrentTime.getMillis)
  }
  doAfterSpec {
    DateTimeUtils.setCurrentMillisSystem
  }

  implicit def string2BigDecimal(amount: String) = BigDecimal(amount)

  "new invoice" should {
    val invoice = Invoice.create(1)
    "be created" in {
      invoice.changes must contain(InvoiceCreated(1))
    }
  }

  "ready to send invoice" should {
    "generate invoice sent event" in {
      val invoice = Invoice.loadFromHistory[DraftInvoice](Seq(
        InvoiceCreated(1),
        InvoiceRecipientChanged(1, Some("Erik")),
        InvoiceItemAdded(1, InvoiceItem(1, "Food", 2.95), 2.95)))

      invoice.send.changes must contain(
        InvoiceSent(1,
          sentDate = new LocalDate(2011, 1, 29),
          dueDate = new LocalDate(2011, 2, 12)))
    }
  }

  "draft invoice" should {
    val invoice: DraftInvoice = Invoice.loadFromHistory(Seq(InvoiceCreated(1)))

    "support changing the recipient" in {
      invoice.changeRecipient(Some("Erik"))
        .changes must contain(InvoiceRecipientChanged(1, Some("Erik")))
    }

    "support adding invoice items" in {
      val updated = invoice.addItem("Food", "2.95") flatMap (_.addItem("Water", "1.95")) flatMap (_.removeItem(1))

      updated.changes must contain(InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95"))
      updated.changes must contain(InvoiceItemAdded(1, InvoiceItem(2, "Water", "1.95"), "4.90"))
      updated.changes must contain(InvoiceItemRemoved(1, InvoiceItem(1, "Food", "2.95"), "1.95"))
    }

    "not be ready to send" in {
      invoice.send.rejected must beEqualTo("recipient and items must be specified before sending")
    }
  }

  "fully specified draft invoice" should {
    val invoice = Invoice.loadFromHistory[DraftInvoice](Seq(
      InvoiceCreated(1),
      InvoiceRecipientChanged(1, Some("Erik")),
      InvoiceItemAdded(1, InvoiceItem(1, "Food", "2.95"), "2.95")))

    "be ready to send" in {
      invoice.send.changes must contain(InvoiceSent(1,
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
      invoice.pay.changes must contain(InvoicePaymentReceived(1, new LocalDate(2011, 1, 29)))
    }

    "be late when dueDate is in the past" in {
      invoice.remind.rejected must beEqualTo("invoice must be late for reminder")

      DateTimeUtils.setCurrentMillisFixed(new LocalDate(2011, 2, 20).toDateTimeAtStartOfDay.getMillis)

      invoice.remind.changes must contain(InvoiceReminderSent(1, new LocalDate(2011, 2, 20)))
    }
  }

}