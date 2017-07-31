package org.example

trait Fuz {
  def abc(): Unit
  def blu(): Unit
  def bla(): Unit
}

class Buz extends Fuz {
  def abc(): Unit = {
    println("abc")
  }
  def blu(): Unit = abc()
  def bla(): Unit = abc()
}