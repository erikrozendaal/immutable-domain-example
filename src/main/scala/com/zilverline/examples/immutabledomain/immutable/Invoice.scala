package com.zilverline.examples.immutabledomain.immutable

import org.joda.time.LocalDate
import com.zilverline.examples.immutabledomain.events._

object Invoice extends AggregateFactory[Invoice, InvoiceEvent] {
  def create(invoiceId: Int) = applyEvent(InvoiceCreated(invoiceId))

  def applyEvent = {
    case event: InvoiceCreated => Invoice(event :: Nil, event.invoiceId)
    case event => unhandled(event)
  }
}

case class Invoice private (
                             uncommittedEvents: List[InvoiceEvent],
                             id: Int,
                             recipient_? : Boolean = false,
                             nextItemId: Int = 1,
                             items: Map[Int, InvoiceItem] = Map.empty,
                             sent_? : Boolean = false,
                             paid_? : Boolean = false,
                             dueDate: Option[LocalDate] = None)
  extends AggregateRoot[Invoice, InvoiceEvent] {

  def changeRecipient(recipient: Option[String]): Invoice = {
    require(!sent_?, "recipient cannot be changed after invoice is sent")
    applyEvent(InvoiceRecipientChanged(id, recipient.map(_.trim).filter(!_.isEmpty)))
  }

  def addItem(description: String, amount: BigDecimal): Invoice = {
    require(!sent_?, "item cannot be added after invoice is sent")
    val item = InvoiceItem(nextItemId, description, amount)
    applyEvent(InvoiceItemAdded(id, item, totalAmount + amount))
  }

  def removeItem(itemId: Int): Invoice = {
    require(!sent_?, "item cannot be removed after invoice is sent")
    val item = items(itemId)
    applyEvent(InvoiceItemRemoved(id, item, totalAmount - item.amount))
  }

  private def totalAmount = items.values.map(_.amount).sum

  private def items_? = items.nonEmpty

  def readyToSend_? = recipient_? && items_? && !sent_?

  def send: Invoice = {
    require(!sent_?, "invoice already sent")
    require(readyToSend_?, "recipient and items must be specified before sending")
    val now = new LocalDate
    applyEvent(InvoiceSent(id, sentDate = now, dueDate = now.plusDays(14)))
  }

  def readyToPay_? = sent_? && !paid_?

  def pay: Invoice = {
    require(sent_?, "invoice cannot be paid before sending")
    require(!paid_?, "invoice already paid")
    applyEvent(InvoicePaymentReceived(id, new LocalDate))
  }

  def late_? = readyToPay_? && dueDate.get.isBefore(new LocalDate)

  def remind: Invoice = {
    require(late_?, "invoice must be late for reminder")
    applyEvent(InvoiceReminderSent(id, new LocalDate))
  }

  def markCommitted = copy(uncommittedEvents = Nil)

  def applyEvent = {
    case event: InvoiceRecipientChanged =>
      copy(event :: uncommittedEvents, recipient_? = event.recipient.isDefined)
    case event: InvoiceItemAdded =>
      copy(event :: uncommittedEvents,
        items = items + (event.item.id -> event.item),
        nextItemId = nextItemId + 1)
    case event: InvoiceItemRemoved =>
      copy(event :: uncommittedEvents, items = items - event.item.id)
    case event: InvoiceSent =>
      copy(event :: uncommittedEvents, sent_? = true, dueDate = Some(event.dueDate))
    case event: InvoicePaymentReceived =>
      copy(event :: uncommittedEvents, paid_? = true)
    case event: InvoiceReminderSent =>
      copy(event :: uncommittedEvents)
    case event => unhandled(event)
  }
}
