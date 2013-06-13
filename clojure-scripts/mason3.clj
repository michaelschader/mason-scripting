(let [tutorial1 (proxy [sim.app.tutorial1and2.Tutorial1] [0]

	  (start []
		(println "using overridden start()")
		;(proxy-super start)
		(set! (.grid this) (sim.field.grid.IntGrid2D. (.gridWidth this) (.gridHeight this)))
		(.seedGrid this)
		(.scheduleRepeating (.schedule this) (sim.app.tutorial1and2.CA.))))]

	(defn display [field]
		(dotimes [y (count field)]
			(dotimes [x (count (aget field y))]
				(print (aget field x y)))
			(println)))

	(.start tutorial1)

	(while (< (.. tutorial1 schedule getSteps) 5000)
		(.. tutorial1 schedule (step tutorial1))
		(let [steps (.. tutorial1 schedule getSteps)]
			(if (= 0 (mod steps 500))
	        	(println (str "Steps: " steps " Time: " (.. tutorial1 schedule getTime))))))

	(display (.. tutorial1 grid field))

	(.finish tutorial1))
