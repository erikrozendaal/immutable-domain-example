package com.zilverline.examples.immutabledomain.immutable

trait AggregateRoot[AR <: AggregateRoot[AR, Event], Event] {
  def applyEvent: Event => AR

  def uncommittedEvents: List[Event]

  def markCommitted: AR
}

trait AggregateFactory[AR <: AggregateRoot[AR, Event], Event] {
  protected def applyCreationEvent: Event => AR

  def loadFromHistory(history: Iterable[Event]): AR = {
    val aggregate = applyCreationEvent(history.head)
    history.tail.foldLeft(aggregate)(_.applyEvent(_)).markCommitted
  }
}
