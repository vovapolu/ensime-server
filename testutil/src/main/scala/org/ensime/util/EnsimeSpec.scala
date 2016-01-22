// Copyright (C) 2016 ENSIME Authors
// License: GPL 3.0
package org.ensime.util

import org.scalatest._
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler

/**
 * Boilerplate remover and preferred testing style in ENSIME.
 */
trait EnsimeSpec extends FlatSpec with Inside {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()
  val log = LoggerFactory.getLogger(this.getClass)
}
