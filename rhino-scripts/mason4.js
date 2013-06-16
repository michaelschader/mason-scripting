
var MAX = 1000;

var mtime = function(fun) {
	var _start = java.lang.System.currentTimeMillis();
	fun();
	var _end = java.lang.System.currentTimeMillis();
	return (_end - _start) / 1000;
}

function setCell(agrid, x, y, value) {
	Packages.sim.scripting.ScriptHelper.set(agrid, x, y, value);
}
function lcount(agrid, x, y) {
	return Packages.sim.scripting.ScriptHelper.lcount(agrid, x, y);
}

var gridWidth = 100;
var gridHeight = 100;
var grid = new Packages.sim.field.grid.IntGrid2D(gridWidth, gridHeight); 
var tempGrid = new Packages.sim.field.grid.IntGrid2D(0, 0);

var b_heptomino = 	[[0, 1, 1], 
		 			 [1, 1, 0],
		 			 [0, 1, 1],
		 			 [0, 0, 1]];
		
var ca = new Packages.sim.engine.Steppable({
	step: function(state) {
		tempGrid.setTo(grid);
		for (var x = 0; x < gridWidth; x++) {
			for (var y = 0; y < gridHeight; y++) {
				var count = lcount(tempGrid, x, y);
				if (count <= 2 || count >= 5) {
					setCell(grid, x, y, 0);
				} else if (count == 3) {
					setCell(grid, x, y, 1);
				}
			}
		}
	}
});

var tutorial1 = new JavaAdapter(Packages.sim.engine.SimState, {
	start: function() {
		for (var x = 0; x < b_heptomino.length; x++) {
			for (var y = 0; y < b_heptomino[x].length; y++) {
				grid.field[Math.floor(x + grid.field.length / 2 + b_heptomino.length / 2)]
						  [Math.floor(y + grid.field[x].length / 2 + b_heptomino[x].length / 2)]
						=  b_heptomino[x][y];
			}
		}
		this.schedule.scheduleRepeating(ca);
	}
}, 0);
	
print(MAX / mtime(function() {
	tutorial1.start();

	while (tutorial1.schedule.getSteps() < MAX) {
		tutorial1.schedule.step(tutorial1);
		var steps = tutorial1.schedule.getSteps();
//		if (steps % 100 == 0) { print("Steps: " + steps); }
	}

	tutorial1.finish();
}));
