package com.zilverline.examples.immutabledomain.jpa

import javax.persistence.{Persistence, EntityManager, EntityManagerFactory}

object JpaSupport {
  lazy val entityManagerFactory: EntityManagerFactory = Persistence.createEntityManagerFactory("manager")

  def withEntityManager(callback: EntityManager => Unit)(implicit emf: EntityManagerFactory) {
    val em = emf.createEntityManager
    try {
      em.getTransaction.begin
      try {
        callback(em)
      } finally {
        if (em.getTransaction.getRollbackOnly) {
          em.getTransaction.rollback
        } else {
          em.getTransaction.commit
        }
      }
    } finally {
      em.close
    }
  }
}