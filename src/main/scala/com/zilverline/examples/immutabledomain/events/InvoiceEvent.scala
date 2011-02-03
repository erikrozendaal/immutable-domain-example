package com.zilverline.examples.immutabledomain.events

import org.joda.time.LocalDate

case class InvoiceItem(id: Int, description: String, amount: BigDecimal)

sealed trait InvoiceEvent {
  val invoiceId: Int
}

case class InvoiceCreated(invoiceId: Int) extends InvoiceEvent
case class InvoiceRecipientChanged(invoiceId: Int, recipient: Option[String]) extends InvoiceEvent
case class InvoiceItemAdded(invoiceId: Int, item: InvoiceItem, totalAmount: BigDecimal) extends InvoiceEvent
case class InvoiceItemRemoved(invoiceId: Int, item: InvoiceItem, totalAmount: BigDecimal) extends InvoiceEvent
case class InvoiceSent(invoiceId: Int, sentDate: LocalDate, dueDate: LocalDate) extends InvoiceEvent
case class InvoiceReminderSent(invoiceId: Int, reminderDate: LocalDate) extends InvoiceEvent
case class InvoicePaymentReceived(invoiceId: Int, paymentDate: LocalDate) extends InvoiceEvent
