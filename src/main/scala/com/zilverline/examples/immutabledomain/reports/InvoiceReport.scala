package com.zilverline.examples.immutabledomain.reports

import org.joda.time.LocalDate
import collection.immutable.SortedMap
import com.zilverline.examples.immutabledomain.events._

case class InvoiceReport(
                          id: Int,
                          recipient: Option[String] = None,
                          items: SortedMap[Int, InvoiceItem] = SortedMap.empty,
                          totalAmount: BigDecimal = 0,
                          sentDate: Option[LocalDate] = None,
                          dueDate: Option[LocalDate] = None,
                          reminderDates: List[LocalDate] = Nil,
                          paymentDate: Option[LocalDate] = None) {

  def handleEvent(event: InvoiceEvent): InvoiceReport = event match {
    case event: InvoiceCreated =>
      error("unexpected event")
    case event: InvoiceRecipientChanged =>
      copy(recipient = event.recipient)
    case event: InvoiceItemAdded =>
      copy(totalAmount = event.totalAmount, items = items + (event.item.id -> event.item))
    case event: InvoiceItemRemoved =>
      copy(totalAmount = event.totalAmount, items = items - event.item.id)
    case event: InvoiceSent =>
      copy(sentDate = Some(event.sentDate), dueDate = Some(event.dueDate))
    case event: InvoiceReminderSent =>
      copy(reminderDates = event.reminderDate :: reminderDates)
    case event: InvoicePaymentReceived =>
      copy(paymentDate = Some(event.paymentDate))
  }
}

object InvoiceReport {
  def handleEvent(event: InvoiceCreated) = InvoiceReport(event.invoiceId)
}
