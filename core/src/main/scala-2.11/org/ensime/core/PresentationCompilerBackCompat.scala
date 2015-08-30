// Copyright (C) 2015 Sam Halliday
// License: see the LICENSE file
package org.ensime.core

import Predef.{any2stringadd => _, _}

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.SourceFile

/**
 * Simulate methods that were added in later versions of the scalac
 * API, or to generate fake methods that we can use in both versions.
 */
trait PresentationCompilerBackCompat {
  this: RichPresentationCompiler =>

  implicit class RichPosition(pos: Position) {
    // annoyingly, endOrPoint is deprecated
    def endOrCursor: Int = pos.end
  }

}
