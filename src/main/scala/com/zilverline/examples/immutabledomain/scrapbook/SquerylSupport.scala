package com.zilverline.examples.immutabledomain.scrapbook

import org.squeryl._
import adapters.H2Adapter

object SquerylSupport {
  Class.forName("org.h2.Driver")

  SessionFactory.concreteFactory = Some(() =>
    Session.create(
      java.sql.DriverManager.getConnection("jdbc:h2:mem:db;DB_CLOSE_DELAY=-1"),
      new H2Adapter))

  PrimitiveTypeMode.using(SessionFactory.newSession) {
    //InvoiceReport.create
  }
}