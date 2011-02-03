package com.zilverline.examples.immutabledomain.immutable

import org.joda.time.LocalDate

class Invoice extends AggregateRoot[InvoiceEvent] {
  private var id: Int = _

  private var recipient_? = false

  private var nextItemId = 1

  private var items: Map[Int, InvoiceItem] = Map.empty

  private var sent_? = false

  private var paid_? = false

  private var dueDate: Option[LocalDate] = None

  def this(id: Int) {
    this()
    record(InvoiceCreated(id))
  }

  def changeRecipient(recipient: Option[String]) {
    require(!sent_?, "recipient cannot be changed after invoice is sent")
    record(InvoiceRecipientChanged(id, recipient.map(_.trim).filter(!_.isEmpty)))
  }

  def addItem(description: String, amount: BigDecimal) {
    require(!sent_?, "item cannot be added after invoice is sent")
    val item = InvoiceItem(nextItemId, description, amount)
    record(InvoiceItemAdded(id, item, totalAmount + amount))
  }

  def removeItem(itemId: Int) {
    require(!sent_?, "item cannot be removed after invoice is sent")
    val item = items(itemId)
    record(InvoiceItemRemoved(id, item, totalAmount - item.amount))
  }

  private def totalAmount = items.values.map(_.amount).sum

  private def items_? = items.nonEmpty

  def readyToSend_? = recipient_? && items_? && !sent_?

  def send {
    require(!sent_?, "invoice already sent")
    require(readyToSend_?, "recipient and items must be specified before sending")
    val now = new LocalDate
    record(InvoiceSent(id, sentDate = now, dueDate = now.plusDays(14)))
  }

  def readyToPay_? = sent_? && !paid_?

  def pay {
    require(sent_?, "invoice cannot be paid before sending")
    require(!paid_?, "invoice already paid")
    record(InvoicePaymentReceived(id, new LocalDate))
  }

  def late_? = readyToPay_? && dueDate.get.isBefore(new LocalDate)

  def remind {
    require(late_?, "invoice must be late for reminder")
    record(InvoiceReminderSent(id, new LocalDate))
  }

  protected def applyEvent = {
    case event: InvoiceCreated =>
      id = event.invoiceId
    case event: InvoiceRecipientChanged =>
      recipient_? = event.recipient.isDefined
    case event: InvoiceItemAdded =>
      items += event.item.id -> event.item
      nextItemId += 1
    case event: InvoiceItemRemoved =>
      items -= event.item.id
    case event: InvoiceSent =>
      sent_? = true
      dueDate = Some(event.dueDate)
    case event: InvoicePaymentReceived =>
      paid_? = true
    case event: InvoiceReminderSent =>
  }
}