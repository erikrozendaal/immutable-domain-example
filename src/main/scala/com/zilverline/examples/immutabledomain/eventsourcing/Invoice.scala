package com.zilverline.examples.immutabledomain.eventsourcing

import org.joda.time.LocalDate

case class InvoiceItem(id: Int, description: String, amount: BigDecimal)

trait InvoiceEvent {
  val invoiceId: Int
}

case class InvoiceCreated(invoiceId: Int) extends InvoiceEvent
case class InvoiceRecipientChanged(invoiceId: Int, recipient: Option[String]) extends InvoiceEvent
case class InvoiceItemAdded(invoiceId: Int, item: InvoiceItem, totalAmount: BigDecimal) extends InvoiceEvent
case class InvoiceItemRemoved(invoiceId: Int, item: InvoiceItem, totalAmount: BigDecimal) extends InvoiceEvent
case class InvoiceSent(invoiceId: Int, sentDate: LocalDate, dueDate: LocalDate) extends InvoiceEvent
case class InvoicePaid(invoiceId: Int, paymentDate: LocalDate) extends InvoiceEvent
case class InvoiceReminderSent(invoiceId: Int, reminderDate: LocalDate) extends InvoiceEvent

class Invoice extends AggregateRoot[InvoiceEvent] {
  private var _id: Int = _

  private var _recipient: Option[String] = None

  private var _nextItemId = 1

  private var _items: Map[Int, InvoiceItem] = Map.empty

  private var _sentDate: Option[LocalDate] = None

  private var _dueDate: Option[LocalDate] = None

  private var _paymentDate: Option[LocalDate] = None

  def this(id: Int) {
    this()
    record(InvoiceCreated(id))
  }

  def changeRecipient(recipient: Option[String]) {
    require(!sent_?, "recipient cannot be changed after invoice is sent")
    record(InvoiceRecipientChanged(_id, recipient.map(_.trim).filter(!_.isEmpty)))
  }

  def addItem(description: String, amount: BigDecimal) {
    require(!sent_?, "item cannot be added after invoice is sent")
    val item = InvoiceItem(_nextItemId, description, amount)
    record(InvoiceItemAdded(_id, item, totalAmount + amount))
  }

  def removeItem(itemId: Int) {
    require(!sent_?, "item cannot be removed after invoice is sent")
    val item = _items(itemId)
    record(InvoiceItemRemoved(_id, item, totalAmount - item.amount))
  }

  def readyToSend_? = recipient_? && items_? && !sent_?

  def send {
    require(!sent_?, "invoice already sent")
    require(readyToSend_?, "recipient and items must be specified before sending")
    val now = new LocalDate
    record(InvoiceSent(_id, sentDate = now, dueDate = now.plusDays(14)))
  }

  def readyToPay_? = sent_? && !paid_?

  def pay {
    require(sent_?, "invoice cannot be paid before sending")
    require(!paid_?, "invoice already paid")
    record(InvoicePaid(_id, new LocalDate))
  }

  def late_? = readyToPay_? && _dueDate.get.isBefore(new LocalDate)

  def remind {
    require(late_?, "invoice must be late for reminder")
    record(InvoiceReminderSent(_id, new LocalDate))
  }

  private def totalAmount = _items.values.map(_.amount).sum

  private def recipient_? = _recipient.isDefined

  private def items_? = _items.nonEmpty

  private def sent_? = _sentDate.isDefined

  private def paid_? = _paymentDate.isDefined

  protected def applyEvent = {
    case InvoiceCreated(invoiceId) =>
      _id = invoiceId
    case InvoiceRecipientChanged(_, recipient) =>
      _recipient = recipient
    case InvoiceItemAdded(_, item, totalAmount) =>
      _items += item.id -> item
      _nextItemId += 1
    case InvoiceItemRemoved(_, item, totalAmount) =>
      _items -= item.id
    case InvoiceSent(_, sentDate, dueDate) =>
      _sentDate = Some(sentDate)
      _dueDate = Some(dueDate)
    case InvoicePaid(_, paymentDate) =>
      _paymentDate = Some(paymentDate)
    case InvoiceReminderSent(_, _) =>
  }
}
