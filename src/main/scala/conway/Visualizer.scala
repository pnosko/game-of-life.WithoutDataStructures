package conway

import rx.lang.scala._
import scala.language.postfixOps
import scala.concurrent.duration._
import conway.core.GameOfLife._

case class Point(x: Int, y: Int)
case class Rectangle(topLeft: Point, bottomRight: Point)

class Visualizer(window: Rectangle) {
  
  def display(state: State): State = {
    println()
    val str = stringify(state)
    println(str)
    state
  }
  
  def play(state: State, interval: Duration = 200 milliseconds): Observable[State] = {
    Observable.interval(interval).scan(state)((st, time) => advanceOne(st))
  }
  
  def advanceOne(state: State): State = {
    display(evolveGeneration(window.topLeft.x, window.topLeft.y, window.bottomRight.x, window.bottomRight.y, state))
  }
  
  def stringify(state: State): String = 
    (for {
      x <- window.topLeft.x to window.bottomRight.x
      y <- window.topLeft.y to window.bottomRight.y
    } yield { 
      val res = if (y == 0) "\r\n" else "" 
      res + (if (isAlive(x, y, state)) "x" else "_")
    }).mkString
}