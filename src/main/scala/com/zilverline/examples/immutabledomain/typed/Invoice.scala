package com.zilverline.examples.immutabledomain.typed

import org.joda.time.LocalDate
import com.zilverline.examples.immutabledomain.events._

object Invoice extends AggregateFactory[Invoice, InvoiceEvent] {
  def create(invoiceId: Int) = applyCreated(InvoiceCreated(invoiceId))

  def applyEvent = {
    case event: InvoiceCreated => applyCreated(event)
    case event => unhandled(event)
  }

  private def applyCreated(event: InvoiceCreated) = DraftInvoice(event :: Nil, event.invoiceId)
}

sealed trait Invoice extends AggregateRoot[Invoice, InvoiceEvent]

case class DraftInvoice(
                         uncommittedEvents: List[InvoiceEvent],
                         id: Int,
                         recipient_? : Boolean = false,
                         nextItemId: Int = 1,
                         items: Map[Int, InvoiceItem] = Map.empty)
  extends Invoice {

  def changeRecipient(recipient: Option[String]): DraftInvoice = {
    applyRecipientChanged(InvoiceRecipientChanged(id, recipient.map(_.trim).filter(!_.isEmpty)))
  }

  def addItem(description: String, amount: BigDecimal): DraftInvoice = {
    val item = InvoiceItem(nextItemId, description, amount)
    applyItemAdded(InvoiceItemAdded(id, item, totalAmount + amount))
  }

  def removeItem(itemId: Int): DraftInvoice = {
    val item = items(itemId)
    applyItemRemoved(InvoiceItemRemoved(id, item, totalAmount - item.amount))
  }

  private def totalAmount = items.values.map(_.amount).sum

  private def items_? = items.nonEmpty

  def readyToSend_? = recipient_? && items_?

  def send: SentInvoice = {
    require(readyToSend_?, "recipient and items must be specified before sending")
    val now = new LocalDate
    applySent(InvoiceSent(id, sentDate = now, dueDate = now.plusDays(14)))
  }

  def markCommitted = copy(uncommittedEvents = Nil)

  def applyEvent = {
    case event: InvoiceRecipientChanged => applyRecipientChanged(event)
    case event: InvoiceItemAdded => applyItemAdded(event)
    case event: InvoiceItemRemoved => applyItemRemoved(event)
    case event: InvoiceSent => applySent(event)
    case event => unhandled(event)
  }

  private def applyRecipientChanged(event: InvoiceRecipientChanged) = copy(event :: uncommittedEvents, recipient_? = event.recipient.isDefined)

  private def applyItemAdded(event: InvoiceItemAdded) = copy(event :: uncommittedEvents,
    items = items + (event.item.id -> event.item),
    nextItemId = nextItemId + 1)

  private def applyItemRemoved(event: InvoiceItemRemoved) = copy(event :: uncommittedEvents, items = items - event.item.id)

  private def applySent(event: InvoiceSent) = new SentInvoice(event :: uncommittedEvents, id, event.dueDate)
}

case class SentInvoice(
                        uncommittedEvents: List[InvoiceEvent],
                        id: Int,
                        dueDate: LocalDate)
  extends Invoice {

  def pay: PaidInvoice = {
    applyPaymentReceived(InvoicePaymentReceived(id, new LocalDate))
  }

  def late_? = dueDate.isBefore(new LocalDate)

  def remind: SentInvoice = {
    require(late_?, "invoice must be late for reminder")
    applyReminderSent(InvoiceReminderSent(id, new LocalDate))
  }

  def markCommitted = copy(uncommittedEvents = Nil)

  def applyEvent = {
    case event: InvoicePaymentReceived => applyPaymentReceived(event)
    case event: InvoiceReminderSent => applyReminderSent(event)
    case event => unhandled(event)
  }

  private def applyPaymentReceived(event: InvoicePaymentReceived) = PaidInvoice(event :: uncommittedEvents)

  private def applyReminderSent(event: InvoiceReminderSent) = copy(event :: uncommittedEvents)
}

case class PaidInvoice(uncommittedEvents: List[InvoiceEvent]) extends Invoice {
  def markCommitted = copy(uncommittedEvents = Nil)

  def applyEvent = unhandled
}
