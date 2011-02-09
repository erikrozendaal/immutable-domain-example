package com.zilverline.examples.immutabledomain.typedmonadic

import org.joda.time.LocalDate
import com.zilverline.examples.immutabledomain.events._
import Behaviors._

object Invoice extends AggregateFactory[Invoice, InvoiceEvent] {
  def create(invoiceId: Int) = applyCreated(InvoiceCreated(invoiceId))

  def applyEvent = applyCreated

  private def applyCreated = handler {event: InvoiceCreated => DraftInvoice(event.invoiceId)}
}

sealed trait Invoice extends AggregateRoot[InvoiceEvent]

case class DraftInvoice(
                         id: Int,
                         recipient_? : Boolean = false,
                         nextItemId: Int = 1,
                         items: Map[Int, InvoiceItem] = Map.empty)
  extends Invoice {

  def changeRecipient(recipient: Option[String]): Behavior[DraftInvoice] = {
    applyRecipientChanged(InvoiceRecipientChanged(id, recipient.map(_.trim).filter(_.nonEmpty)))
  }

  def addItem(description: String, amount: BigDecimal): Behavior[DraftInvoice] = {
    val item = InvoiceItem(nextItemId, description, amount)
    applyItemAdded(InvoiceItemAdded(id, item, totalAmount + amount))
  }

  def removeItem(itemId: Int): Behavior[DraftInvoice] = {
    val item = items(itemId)
    applyItemRemoved(InvoiceItemRemoved(id, item, totalAmount - item.amount))
  }

  private def totalAmount = items.values.map(_.amount).sum

  private def items_? = items.nonEmpty

  private def readyToSend_? = recipient_? && items_?

  def send: Behavior[SentInvoice] =
    guard(readyToSend_?, "recipient and items must be specified before sending") flatMap {_ =>
      val now = new LocalDate
      applySent(InvoiceSent(id, sentDate = now, dueDate = now.plusDays(14)))
    }

  def applyEvent = applyRecipientChanged orElse applyItemAdded orElse applyItemRemoved orElse applySent

  private def applyRecipientChanged = handler {
    event: InvoiceRecipientChanged =>
      copy(recipient_? = event.recipient.isDefined)
  }

  private def applyItemAdded = handler {
    event: InvoiceItemAdded =>
      copy(items = items + (event.item.id -> event.item), nextItemId = nextItemId + 1)
  }

  private def applyItemRemoved = handler {
    event: InvoiceItemRemoved =>
      copy(items = items - event.item.id)
  }

  private def applySent = handler {event: InvoiceSent => new SentInvoice(id, event.dueDate)}
}

class SentInvoice(id: Int, dueDate: LocalDate) extends Invoice {
  def pay: Behavior[PaidInvoice] = {
    applyPaymentReceived(InvoicePaymentReceived(id, new LocalDate))
  }

  private def late_? = dueDate.isBefore(new LocalDate)

  def remind: Behavior[SentInvoice] =
    guard(late_?, "invoice must be late for reminder") flatMap {_ =>
      applyReminderSent(InvoiceReminderSent(id, new LocalDate))
    }

  def applyEvent = applyPaymentReceived orElse applyReminderSent

  private def applyPaymentReceived = handler {
    event: InvoicePaymentReceived => new PaidInvoice
  }

  private def applyReminderSent = handler {
    event: InvoiceReminderSent => this
  }
}

class PaidInvoice extends Invoice {
  def applyEvent = unhandled
}
