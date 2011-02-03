package com.zilverline.examples.immutabledomain.immutable

trait AggregateRoot[AR <: AggregateRoot[AR, Event], Event] {this: AR =>
  def applyEvent: Event => AR

  def uncommittedEvents: List[Event]

  def markCommitted: AR
}

trait AggregateFactory[AR <: AggregateRoot[AR, Event], Event] {
  protected def applyEvent: Event => AR

  def loadFromHistory(history: Iterable[Event]): AR = {
    val aggregate = applyEvent(history.head)
    history.tail.foldLeft(aggregate)(_.applyEvent(_)).markCommitted
  }
}
