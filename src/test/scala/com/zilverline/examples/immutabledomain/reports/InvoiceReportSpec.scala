package com.zilverline.examples.immutabledomain.reports

import org.specs.Specification
import org.joda.time.LocalDate
import com.zilverline.examples.immutabledomain.events._
import org.specs.runner.JUnit4

class InvoiceReportTest extends JUnit4(InvoiceReportSpec)

object InvoiceReportSpec extends Specification {

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