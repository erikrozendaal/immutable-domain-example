package com.zilverline.examples.immutabledomain.immutable

trait EventSourced[ES <: EventSourced[ES, Event], Event] {
  def applyEvent: Event => ES
}

trait AggregateRoot[AR <: AggregateRoot[AR, Event], Event] extends EventSourced[AR, Event] {
  def uncommittedEvents: List[Event]

  def markCommitted: AR
}

trait AggregateFactory[AR <: AggregateRoot[AR, Event], Event] extends EventSourced[AR, Event] {
  def loadFromHistory(history: Iterable[Event]): AR = {
    var aggregate = applyEvent(history.head)
    for (event <- history.tail)
      aggregate = aggregate.applyEvent(event)
    aggregate.markCommitted
  }
}
