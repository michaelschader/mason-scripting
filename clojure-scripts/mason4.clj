(let [gridWidth 100
	  gridHeight 100

	  b_heptomino 	[[0 1 1]
		 			 [1 1 0]
		 			 [0 1 1]
		 			 [0 0 1]]

	  tutorial1 (proxy [sim.app.tutorial1and2.Tutorial1] [0]
		
		(start []
			(println "using overridden start()")
			;(proxy-super start)
			(set! (.grid this) (sim.field.grid.IntGrid2D. gridWidth gridHeight))
			(.seedGrid this)
			(.scheduleRepeating (.schedule this) (sim.app.tutorial1and2.CA.)))
			
		(seedGrid [] 
			(println "using overridden seedGrid()")
			(dotimes [x (count b_heptomino)] 
				(dotimes [y (count (nth b_heptomino x))] 
					(aset (.. this grid field) 
						  (- (+ x (/ (count (.. this grid field)) 2)) (/ (count b_heptomino) 2))
						  (- (+ y (/ (count (aget (.. this grid field) x)) 2)) (/ (count (nth b_heptomino x)) 2))
						  (int (nth (nth b_heptomino x) y)))))))]

	(defn display [field]
		(dotimes [y gridHeight]
			(dotimes [x gridWidth]
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
