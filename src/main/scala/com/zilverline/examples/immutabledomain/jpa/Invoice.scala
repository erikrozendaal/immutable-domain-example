package com.zilverline.examples.immutabledomain.jpa

import javax.persistence._
import scala.collection.JavaConversions._
import java.math.BigDecimal
import java.util.{ArrayList, Collections, Date, List}
import org.joda.time.{DateTimeUtils, LocalDate}

@Entity
class InvoiceItem {
  @Id
  @GeneratedValue
  private var _id: Int = _

  @ManyToOne
  private var _invoice: Invoice = _

  @Basic(optional = false)
  private var _description: String = _

  @Basic(optional = false)
  private var _amount: BigDecimal = _

  def this(invoice: Invoice, description: String, amount: BigDecimal) {
    this ()
    _invoice = invoice
    _description = description
    _amount = amount
  }

  def description = _description

  def amount = _amount
}

@Entity
class Invoice {
  @Id
  @GeneratedValue
  private var _id: Int = _

  private var _recipient: String = _

  @OneToMany(cascade = Array(CascadeType.ALL))
  @OrderBy
  private var _items: List[InvoiceItem] = new ArrayList

  @Basic(optional = false)
  private var _totalAmount: BigDecimal = BigDecimal.ZERO

  @Temporal(TemporalType.DATE)
  private var _sentDate: Date = _

  @Temporal(TemporalType.DATE)
  private var _dueDate: Date = _

  @Temporal(TemporalType.DATE)
  private var _paymentDate: Date = _

  @Temporal(TemporalType.DATE)
  private var _reminderDate: Date = _

  def recipient_? = _recipient != null

  def recipient = Option(_recipient)

  def recipient_=(recipient: Option[String]) {
    require(!sent_?, "recipient cannot be changed after invoice is sent")
    _recipient = recipient.map(_.trim).filter(!_.isEmpty).orNull
  }

  def totalAmount = _totalAmount

  def items_? = _items.nonEmpty

  def items = Collections.unmodifiableList(_items)

  def addItem(description: String, amount: BigDecimal) {
    require(!sent_?, "items cannot be changed after invoice is sent")
    _totalAmount = _totalAmount.add(amount)
    _items.add(new InvoiceItem(this, description, amount))
  }

  def removeItem(index: Int) {
    require(!sent_?, "items cannot be changed after invoice is sent")
    val item = _items.remove(index)
    _totalAmount = _totalAmount.subtract(item.amount)
  }

  def sent_? = _sentDate != null

  def readyToSend_? = recipient_? && items_? && !sent_?

  def send {
    require(!sent_?, "invoice already sent")
    require(readyToSend_?, "recipient and items must be specified before sending")
    val now = new LocalDate
    _sentDate = now.toDateTimeAtStartOfDay.toDate
    _dueDate = now.plusDays(14).toDateTimeAtStartOfDay.toDate
  }

  def readyForPayment_? = sent_? && !paid_?

  def paid_? = _paymentDate != null

  def paymentReceived {
    require(sent_?, "invoice cannot be paid before sending")
    require(!paid_?, "invoice already paid")
    val now = new LocalDate
    _paymentDate = now.toDateTimeAtStartOfDay.toDate
  }

  def late_? = readyForPayment_? && _dueDate.getTime < DateTimeUtils.currentTimeMillis

  def remind {
    require(late_?, "invoice must be late for reminder")
    _reminderDate = new LocalDate().toDateTimeAtStartOfDay.toDate
  }

  def sentDate = if (_sentDate == null) null else new LocalDate(_sentDate)

  def dueDate = if (_dueDate == null) null else new LocalDate(_dueDate)

  def paymentDate = if (_paymentDate == null) null else new LocalDate(_paymentDate)

  def reminderDate = if (_reminderDate == null) null else new LocalDate(_reminderDate)
}
