var tutorial1 = new Packages.sim.app.tutorial1and2.Tutorial1(0);
var MAX = 18000;

var mtime = function(fun) {
	var _start = java.lang.System.currentTimeMillis();
	fun();
	var _end = java.lang.System.currentTimeMillis();
	return (_end - _start) / 1000;
}

print(MAX / mtime(function() {
	tutorial1.start();

	do {
		var steps = tutorial1.schedule.getSteps();
		tutorial1.schedule.step(tutorial1);
		if (steps % (MAX / 10) == 0) {
			print("Steps: " + steps + " Time: " + tutorial1.schedule.getTime());
		}
	} while (steps < MAX);

	tutorial1.finish();
}) + " steps/sec");
