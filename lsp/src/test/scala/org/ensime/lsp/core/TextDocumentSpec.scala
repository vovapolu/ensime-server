// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.lsp.core

import org.ensime.lsp.api.types.Position
import org.scalatest._
import org.scalatest.Matchers._

class TextDocumentSpec extends FreeSpec {
  "one line doc" - {
    val text = """one line document"""

    val td = TextDocument("file:///dummy.txt", text.toArray)

    "should produce correct positions" in {
      td.offsetToPosition(0) shouldEqual Position(0, 0)
      td.offsetToPosition(3) shouldEqual Position(0, 3)
      td.offsetToPosition(16) shouldEqual Position(0, 16)

      td.positionToOffset(Position(0, 0)) shouldEqual 0
      td.positionToOffset(Position(0, 3)) shouldEqual 3
      td.positionToOffset(Position(0, 16)) shouldEqual 16
    }
  }

  "two lines doc" - {
    val text =
      """|line1
         |line2
         |""".stripMargin

    val td = TextDocument("file:///dummy.txt", text.toArray)

    "should produce correct positions" in {
      td.offsetToPosition(0) shouldEqual Position(0, 0)
      td.offsetToPosition(4) shouldEqual Position(0, 4)
      td.offsetToPosition(5) shouldEqual Position(0, 5) // exactly the new line character

      td.offsetToPosition(6) shouldEqual Position(1, 0)
      td.offsetToPosition(7) shouldEqual Position(1, 1)
      td.offsetToPosition(10) shouldEqual Position(1, 4)
      td.offsetToPosition(11) shouldEqual Position(1, 5)

      td.positionToOffset(Position(0, 0)) shouldEqual 0
      td.positionToOffset(Position(0, 4)) shouldEqual 4
      td.positionToOffset(Position(1, 0)) shouldEqual 6
      td.positionToOffset(Position(1, 4)) shouldEqual 10
      td.positionToOffset(Position(1, 5)) shouldEqual 11
    }
  }

  "several lines CR/LF doc" - {
    val text = "line1\r\nline2\r\n"

    val td = TextDocument("file:///dummy.txt", text.toArray)

    "should produce correct positions" in {
      td.offsetToPosition(0) shouldEqual Position(0, 0)
      td.offsetToPosition(4) shouldEqual Position(0, 4)
      td.offsetToPosition(5) shouldEqual Position(0, 5) // exactly the new line character

      td.offsetToPosition(7) shouldEqual Position(1, 0)
      td.offsetToPosition(8) shouldEqual Position(1, 1)
      td.offsetToPosition(11) shouldEqual Position(1, 4)
      td.offsetToPosition(12) shouldEqual Position(1, 5)

      td.positionToOffset(Position(0, 0)) shouldEqual 0
      td.positionToOffset(Position(0, 4)) shouldEqual 4
      td.positionToOffset(Position(1, 0)) shouldEqual 7
      td.positionToOffset(Position(1, 4)) shouldEqual 11
      td.positionToOffset(Position(1, 5)) shouldEqual 12
    }
  }
}
