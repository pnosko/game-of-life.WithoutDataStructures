package conway.core

import scalaz._
import scalaz.Scalaz._

object GameOfLife {
  type State = (Int, Int) => Boolean
  val deadState: State = (_,_) => false		// yes, i know, it does look like an ass
  
  def isAlive(x: Int, y: Int, state: State): Boolean = state(x, y)
  def addCell(x: Int, y: Int, givenState: State): State = (nx, ny) => ((nx == x && ny == y) || givenState(nx, ny))

  def numberOfAliveNeighbors(x: Int, y: Int, state: State): Int = { 
    var count = 0
    for {
      nx <- -1 to 1
      ny <- -1 to 1
      if (!(nx == 0 && ny == 0))
    } if (isAlive(x + nx, y + ny, state)) count += 1
    count
  }
  
  def isAliveInNextGeneration(x: Int, y: Int, state: State): Boolean = {
    val neighbors = numberOfAliveNeighbors(x, y, state)
    if(isAlive(x, y, state)) neighbors == 2 || neighbors == 3
    else neighbors == 3
  }

  def evolveGeneration(minX: Int, minY: Int, maxX: Int, maxY: Int, state: State): State = {
    require(minX < maxX && minY < maxY)
    var nextGen = deadState
    for {
      x <- minX to maxX
      y <- minY to maxY
    } if(isAliveInNextGeneration(x, y, state)) nextGen = addCell(x, y, nextGen)
    nextGen
  }
  
  val exampleState = addCell(4,2, addCell(3,1, addCell(2,3, addCell(2,2, addCell(1,1, deadState)))))
}