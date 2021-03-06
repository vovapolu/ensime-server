// Copyright: 2010 - 2016 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.util

import scala.Predef.{ any2stringadd => _, _ }

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  import list._

  "list._" should "provide initLast" in {
    List(1).initLast shouldBe ((Nil, 1))
    List(1, 2, 3, 4).initLast shouldBe ((List(1, 2, 3), 4))
  }

  val tuples = List(
    (1, 'a),
    (1, 'b),
    (1, 'c),
    (2, 'b),
    (2, 'b)
  )

  val multi = Map(
    1 -> Set('a, 'b, 'c),
    2 -> Set('b)
  )

  it should "gather multimap sets" in {
    tuples.toMultiMapSet shouldEqual multi
  }

  it should "provide distinctBy" in {
    tuples.distinctBy(_._1) shouldBe List((1, 'a), (2, 'b))
    tuples.distinctBy(_._2) shouldBe List((1, 'a), (1, 'b), (1, 'c))
  }
}
