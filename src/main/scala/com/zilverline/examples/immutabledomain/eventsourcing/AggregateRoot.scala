package com.zilverline.examples.immutabledomain.eventsourcing

import scala.collection._

trait AggregateRoot[Event] {
  protected def applyEvent: Event => Unit

  def uncommittedEvents: Iterable[Event] = _uncommittedEvents

  def markCommitted = _uncommittedEvents.clear

  def loadFromHistory(history: Traversable[Event]) = history.foreach(applyEvent)

  protected def record(event: Event) {
    applyEvent(event)
    _uncommittedEvents += event
  }

  private val _uncommittedEvents = mutable.Queue[Event]()
}
