// Copyright 2013 by rlegendi
// No license :-)

import sim.field.grid.IntGrid2D
import sim.engine.{Steppable, SimState}
import sim.scripting.ScriptHelper.{set => setCell, lcount}

val MAX = 1000

val gridWidth = 100
val gridHeight = 100
val grid = new IntGrid2D(gridWidth, gridHeight)
val tempGrid = new IntGrid2D(0, 0)

val b_heptomino = Array(Array(0, 1, 1), Array(1, 1, 0), Array(0, 1, 1), Array(0, 0, 1))

class Ca extends Steppable {
	def step(state: SimState) = {
		tempGrid.setTo(grid);
		for (x <- 0 until gridWidth;
         y <- 0 until gridHeight;
         count = lcount(tempGrid, x, y))
      if (count <= 2 || count >= 5) {
        setCell(grid, x, y, 0)
      } else if (count == 3) {
        setCell(grid, x, y, 1)
      }
  }
}

class Tutorial1(seed: Int) extends SimState(seed) {
	override def start() = {
		for (x <- 0 until b_heptomino.length;
			   y <- 0 until b_heptomino(x).length)
				grid.field(x + grid.field.length / 2 + b_heptomino.length / 2)((y + grid.field(x).length / 2 + b_heptomino(x).length / 2)) = b_heptomino(x)(y)
		schedule.scheduleRepeating(new Ca())
  }
}

var time = -System.currentTimeMillis()
val tutorial1 = new Tutorial1(0)
tutorial1.start()

//def stateAsFancyString() = // This is just for visual verification
//  (for (row <- grid.field) yield row.map(e => if (0 == e) "." else "@").mkString).mkString("\n")

while (tutorial1.schedule.getSteps() < MAX) {
  tutorial1.schedule.step(tutorial1)
  //val steps = tutorial1.schedule.getSteps()
  //if (steps % 100 == 0)
      //println("Steps: " + steps)
  //println(stateAsFancyString())
  //Thread.sleep(1000)
}

time += System.currentTimeMillis()
println(MAX.toDouble / time * 1000)

