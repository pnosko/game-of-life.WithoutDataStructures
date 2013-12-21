package conway

import core.GameOfLife._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import scala.concurrent.duration.Deadline

@RunWith(classOf[JUnitRunner])
class GameOfLifeSuite extends FunSuite {
  val maxX = 5
  val maxY = 5
  
  test("in a dead world, every cell is dead") {
    val state: State = (_, _) => false
    assert(isAlive(1, 1, state) === false)
  }

  test("in a world with a single living cell, one cell is alive") {
    val state: State = addCell(1,1, deadState)

    assert(isAlive(1, 1, state) === true)
    assert(isAlive(0, 0, state) === false)
  }

  test("adding a second cell to a world") {
    val state = addCell(2, 2, addCell(1,1, deadState))
    
    assert(isAlive(2, 2, state) === true)
    assert(isAlive(1, 1, state) === true)
    assert(isAlive(0, 0, state) === false)
  }
  
  test("coupled cell has one neighbor ") {
    val worldWithCellAt1_1: State = (x, y) => (x == 1 && y == 1)
    val state = addCell(2, 2, worldWithCellAt1_1)
    assert(numberOfAliveNeighbors(1, 1, state) === 1)
  }
  
  test("lone cell has no living neighbors") {
    val state: State = (x, y) => (x == 1 && y == 1)
 
    assert(numberOfAliveNeighbors(1, 1, state) === 0)
  }

  test("generate next gen") {
    val state = addCell(3,1, addCell(2,3, addCell(2,2, addCell(1,1, deadState))))
    val expected =  addCell(3,2, addCell(2,2, addCell(2,1, addCell(1,2, deadState))))
    
    def compare(s1: State, s2: State, minX: Int, minY: Int, maxX: Int, maxY: Int): Boolean = {
      var res = true
      for {
        x <- minX to maxX
        y <- minY to maxY
      } if(isAlive(x, y, s1) != isAlive(x, y, s2))
        {
          println("X: " + x + " Y: " + y + " [actual: " + isAlive(x, y, s1) + ", expected: " + isAlive(x, y, s2) + "]")
    	  res = res && false
        }
      res
    }
    
    assert(compare(evolveGeneration(0, 0, maxX, maxY, state), expected, 0, 0, maxX, maxY) === true)
  }
}
