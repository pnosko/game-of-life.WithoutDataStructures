package conway
import rx.lang.scala._
import conway.core.GameOfLife._

object Runner extends App {
  var disp: Visualizer = _
  
  override def main(args: Array[String]): Unit = {
    val initialState = exampleState
    val window = new Rectangle(new Point(0,0), new Point(5, 5))
    disp = new Visualizer(window)
    doSomething(initialState)
  }
  
  def doSomething(state: State): Unit = {
    Console.readChar match {
      //case 'r' => doSomething())
      case 'x' => println("quit")
      case 'n' => doSomething(disp.advanceOne(state))
      case 'd' => doSomething(disp.display(state))
      case _ => println("dunno")
    }
  }
}