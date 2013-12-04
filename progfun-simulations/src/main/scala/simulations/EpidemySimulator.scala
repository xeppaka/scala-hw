package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = createPopulation

  def createPopulation = {
    var persons = List[Person]()
    
    def addPerson(id: Int, infected: Boolean) = {
      val p: Person = new Person(id)
      
      if (infected) {
        p.becomeInfected
      }
      
      afterDelay(0){ p.move }
      
      persons = p :: persons
    }
    
    for (i <- 1 to (population * 0.01).toInt)
      addPerson(i, true)
    for (i <- (population * 0.01).toInt + 1 to population)
      addPerson(i, false)
      
    persons
  }

  def someoneSick(pos: (Int, Int)): Boolean = {
    persons.find(p => p.row == pos._1 && p.col == pos._2 && (p.sick)) match {
      case None => false
      case Some(pp) => true
    } 
  }
  
  def canBeInfected(pos: (Int, Int)): Boolean = {
    persons.find(p => p.row == pos._1 && p.col == pos._2 && (p.sick || p.immune || p.infected)) match {
      case None => false
      case Some(pp) => true
    } 
  }
  
  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def left: (Int, Int) = {
      (row, (col + roomColumns - 1) % 8)
    }
    
    def right: (Int, Int) = {
      (row, (col + 1) % 8)
    }
    
    def top: (Int, Int) = {
      ((row + roomRows - 1) % roomRows, col)
    }
    
    def bottom: (Int, Int) = {
      ((row + 1) % roomRows, col)
    }
    
    def healthy: Boolean = {
      !infected && !sick && !immune && !dead
    }
    
    //
    // to complete with simulation logic
    //
    
    def move(): Unit = {
      if (!dead) {
      var positions = List[(Int, Int)]()
      if (!someoneSick(left))
        positions = left :: positions
      if (!someoneSick(right))
        positions = right :: positions
      if (!someoneSick(top))
        positions = top :: positions
      if (!someoneSick(bottom))
        positions = bottom :: positions
        
      positions match {
	    case Nil =>
	    case x :: xs => {
	      val selectedPos = positions(randomBelow(positions.length))
	      row = selectedPos._1
	      col = selectedPos._2
	    }
	  }
      
      afterDelay(0) { finishMove }
      }
    }

    def finishMove() = {
      if (healthy) {
        if (canBeInfected((row, col))) {
          if (randomBelow(100) < 40) {
            becomeInfected()
          }
        }
      }
      
      afterDelay(randomBelow(5) + 1) { move }
    }
    
    def becomeInfected() = {
      infected = true
      
      afterDelay(6) { becomeSick }
      afterDelay(14) { becomeDead }
      afterDelay(16) { becomeImmune }
      afterDelay(18) { becomeHealthy }
    }
    
    def becomeSick() = {
      sick = true
    }
    
    def becomeDead() = {
      dead = randomBelow(100) < 25
    }
    
    def becomeImmune() = {
      if (!dead) {
        sick = false
        immune = true
      }
    }
    
    def becomeHealthy() = {
      if (!dead) {
        infected = false
        immune = false
      }
    }
  }
}
