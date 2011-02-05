package com.zilverline.examples.immutabledomain.typed

trait EventSourced[ES <: EventSourced[ES, Event], Event] {
  def applyEvent: Event => ES

  def unhandled(event: Event) = error("event " + event + " does not apply to " + this)
}

trait AggregateRoot[AR <: AggregateRoot[AR, Event], Event] extends EventSourced[AR, Event] {
  def uncommittedEvents: List[Event]

  def markCommitted: AR
}

trait AggregateFactory[AR <: AggregateRoot[AR, Event], Event] extends EventSourced[AR, Event] {
  def loadFromHistory[T <: AR](history: Iterable[Event]): T = {
    var aggregate = applyEvent(history.head)
    for (event <- history.tail)
      aggregate = aggregate.applyEvent(event)
    aggregate.markCommitted.asInstanceOf[T]
  }
}
