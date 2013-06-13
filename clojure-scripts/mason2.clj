(set! *warn-on-reflection* true)
(set! *unchecked-math* true)
(def MAX 180000)

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

(let [tutorial1 (sim.app.tutorial1and2.Tutorial1. 0)]

	(.start tutorial1)

	(println (str (/ MAX (mtime
		(while (< (.. tutorial1 schedule getSteps) MAX)
			(.. tutorial1 schedule (step tutorial1))
			(let [steps (.. tutorial1 schedule getSteps)]
				(if (= 0 (mod steps (/ MAX 10)))
		        	(println (str "Steps: " steps " Time: " (.. tutorial1 schedule getTime))))))
	)) " steps/sec")) 

	(.finish tutorial1))
