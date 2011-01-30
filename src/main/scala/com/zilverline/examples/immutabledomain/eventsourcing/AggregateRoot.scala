package com.zilverline.examples.immutabledomain.eventsourcing

import collection.mutable.Queue

trait AggregateRoot[T] {
  protected def applyEvent: T => Unit

  def uncommittedEvents: Iterable[T] = _uncommittedEvents

  def markCommitted = _uncommittedEvents.clear

  def loadFromHistory(history: Traversable[T]) = history foreach applyEvent

  protected def record(event: T) {
    applyEvent(event)
    _uncommittedEvents += event
  }

  private val _uncommittedEvents = Queue[T]()
}
