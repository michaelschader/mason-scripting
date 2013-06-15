(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(def MAX 1000)

(println "Press enter to begin...")
(.readLine (java.io.BufferedReader. (java.io.InputStreamReader. System/in)))

(defmacro mtime [expr]
  	`(let [start# (System/currentTimeMillis)
		   dummy# ~expr
		   end# (System/currentTimeMillis)]
		   (float (/ (- end# start#) 1000))))

(defn display [field]
	(dotimes [y (count (aget field 0))]
		(dotimes [x (count field)]
			(print (aget field x y)))
		(println)))

(defn set-cell [^sim.field.grid.IntGrid2D agrid x y value]
	(sim.scripting.ScriptHelper/set agrid x y value))

(defn lcount [agrid x y] (sim.scripting.ScriptHelper/lcount agrid x y))

(let [	gridWidth 100
	  	gridHeight 100
	  	grid (sim.field.grid.IntGrid2D. gridWidth gridHeight) 
		tempGrid (sim.field.grid.IntGrid2D. 0 0)
	  
		b_heptomino [[0 1 1]
		 			 [1 1 0]
		 			 [0 1 1]
		 			 [0 0 1]]

		ca (proxy [sim.engine.Steppable] []
			(step [state] 
				(.setTo tempGrid grid)
				(dotimes [x (.getWidth tempGrid)]
					(dotimes [y (.getHeight tempGrid)]
						(let [count (lcount tempGrid x y)]
							(if (or (<= count 2) (>= count 5)) 
								(set-cell grid x y 0)
								(if (= count 3)
									(set-cell grid x y 1))))))))

		tutorial1 (proxy [sim.engine.SimState] [0]
			(start []
				(dotimes [x (count b_heptomino)] 
					(dotimes [y (count (nth b_heptomino x))] 
						(aset (.field grid) 
							  (- (+ x (/ (count (.field grid)) 2)) (/ (count b_heptomino) 2))
							  (- (+ y (/ (count (aget (.field grid) x)) 2)) (/ (count (nth b_heptomino x)) 2))
							  (int (nth (nth b_heptomino x) y)))))
				(.scheduleRepeating ^sim.engine.Schedule (.schedule ^sim.engine.SimState this) ca)))

	]
	
	(.start tutorial1)
	
	(println (str (/ MAX (mtime
		(while (< (.. tutorial1 schedule getSteps) MAX)
			(.. tutorial1 schedule (step tutorial1))
			(let [steps (.. tutorial1 schedule getSteps)]
				(if (= 0 (mod steps (/ MAX 10)))
		        	(println (str "Steps: " steps " Time: " (.. tutorial1 schedule getTime))))))
	)) " steps/sec"))

	(.finish tutorial1))
