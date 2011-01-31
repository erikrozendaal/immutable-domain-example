package com.zilverline.examples.immutabledomain.eventsourcing

import org.squeryl.PrimitiveTypeMode._
import org.joda.time.LocalDate
import org.squeryl.{KeyedEntity, Schema, Session}
import java.util.Date

class InvoiceReport(
                     val id: Int,
                     val recipient: Option[String] = None,
                     val totalAmount: BigDecimal = 0,
                     val sentDate: Option[Date] = None,
                     val dueDate: Option[Date] = None,
                     val reminderDate: Option[Date] = None,
                     val paymentDate: Option[Date] = None) {
  def this() = this (-1, Some(""), 0, Some(new Date), Some(new Date), Some(new Date), Some(new Date))
}

object InvoiceReport extends Schema {
  val invoices = table[InvoiceReport]

  def handleEvent(event: InvoiceEvent) = inTransaction {
    Session.currentSession.setLogger(msg => println(msg))
    event match {
      case event: InvoiceCreated =>
        invoices.insert(new InvoiceReport(event.invoiceId))
      case event: InvoiceRecipientChanged =>
        update(invoices) {s => where(s.id === event.invoiceId).set(s.recipient := event.recipient)}
      case event: InvoiceItemAdded =>
        update(invoices) {s => where(s.id === event.invoiceId).set(s.totalAmount := event.totalAmount)}
      case event: InvoiceItemRemoved =>
        update(invoices) {s => where(s.id === event.invoiceId).set(s.totalAmount := event.totalAmount)}
    }
  }

  def lookup(invoiceId: Int): InvoiceReport = inTransaction {
    Session.currentSession.setLogger(msg => println(msg))
    invoices.where(s => s.id === invoiceId).single
  }

}
