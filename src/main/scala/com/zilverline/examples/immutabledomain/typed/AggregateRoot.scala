package com.zilverline.examples.immutabledomain.typed

trait EventSourced[Event] {
  def applyEvent: Event => EventSourced[Event]

  def unhandled(event: Event) = error("event " + event + " does not apply to " + this)
}

trait AggregateRoot[+AR <: AggregateRoot[AR, Event], Event] extends EventSourced[Event] {
  def uncommittedEvents: List[Event]

  def markCommitted: AR
}

trait AggregateFactory[AR <: AggregateRoot[AR, Event], Event] extends EventSourced[Event] {
  def loadFromHistory[T <: AR](history: Iterable[Event]): T = {
    var aggregate = applyEvent(history.head)
    for (event <- history.tail)
      aggregate = aggregate.applyEvent(event)
    aggregate.asInstanceOf[AR].markCommitted.asInstanceOf[T]
  }
}
