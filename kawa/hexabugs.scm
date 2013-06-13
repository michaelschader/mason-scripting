(require 'list-lib)


;;; Begin Library Code

(define-namespace Double2D <sim.util.Double2D>)
(define-namespace Int2D <sim.util.Int2D>)
(define-namespace Continuous2D <sim.field.continuous.Continuous2D>)
(define-namespace SparseGrid2D <sim.field.grid.SparseGrid2D>)
(define-namespace DoubleGrid2D <sim.field.grid.DoubleGrid2D>)
(define-namespace SimState <sim.engine.SimState>)
(define-namespace MersenneTwisterFast <ec.util.MersenneTwisterFast>)
(define-namespace Steppable <sim.engine.Steppable>)
(define-namespace Oriented2D <sim.portrayal.Oriented2D>)
(define-namespace Bag <sim.util.Bag>)
(define-namespace DoubleBag <sim.util.DoubleBag>)
(define-namespace IntBag <sim.util.IntBag>)
(define-namespace Schedule <sim.engine.Schedule>)
(define-namespace GUIState <sim.display.GUIState>)
(define-namespace Display2D <sim.display.Display2D>)
(define-namespace JFrame <javax.swing.JFrame>)
(define-namespace ContinuousPortrayal2D <sim.portrayal.continuous.ContinuousPortrayal2D>)
(define-namespace String <java.lang.String>)
(define-namespace OrientedPortrayal2D <sim.portrayal.simple.OrientedPortrayal2D>)
(define-namespace SimplePortrayal2D <sim.portrayal.SimplePortrayal2D>)
(define-namespace Color <java.awt.Color>)
(define-namespace Controller <sim.display.Controller>)
(define-namespace Console <sim.display.Console>)
(define-namespace System <java.lang.System>)
(define-namespace int <int>)
(define-namespace boolean <boolean>)
(define-namespace double <double>)

(define (/= x y) (not (= x y)))

(define simulation :: SimState #!null)
(define random :: MersenneTwisterFast #!null)
(define schedule :: Schedule #!null)



;;; model properties

(define-simple-class <schemeproperties> (<sim.util.Properties>)
  (properties :: Bag (Bag))
  ((isVolatile) :: <boolean>
   #f)
  ((getObject) :: <Object>
   simulation)
  ((numProperties) :: <int>
   (write (properties:size))
   (properties:size))
  ((getValue index :: <int>) :: <Object>
   ((first (properties:get index))))
  ((_setValue index :: <int> value :: <Object>) :: <Object>
   ((second (properties:get index)) value))
  ((getType index :: <int>) :: <java.lang.Class>  ;; this will be a problem
   (third (properties:get index)))
  ((isReadWrite index :: <int>) :: <boolean>
   (not (eq? #!null (second (properties:get index)))))
  ((getName index :: <int>) :: String
   (fourth (properties:get index)))
  ((getDomain index :: <int>) :: <Object>
   (fifth (properties:get index)))
  ((add-property name type read-function write-function domain)
   (properties:add (list
    read-function
    write-function
    (cond ((eq? type <boolean>) <java.lang.Boolean>:TYPE)
	  ((eq? type <byte>) <java.lang.Byte>:TYPE)
	  ((eq? type <short>) <java.lang.Short>:TYPE)
	  ((eq? type <char>) <java.lang.Character>:TYPE)
	  ((eq? type <int>) <java.lang.Integer>:TYPE)
	  ((eq? type <long>) <java.lang.Long>:TYPE)
	  ((eq? type <float>) <java.lang.Float>:TYPE)
	  ((eq? type <double>) <java.lang.Double>:TYPE)
	  (else type))
    name
    domain))))

(define model-properties :: <schemeproperties> #!null)
(set! model-properties (<schemeproperties>))


(define-syntax define-property
  (syntax-rules (::)
    ((_ var :: type value write-function domain)
     (begin
       (define var :: type value)
       (model-properties:add-property
	(format #f "~a" (quote var))
	type
	(lambda () var)
	(if (eq? #t write-function) 
	    (lambda (val) (set! var val))
	    (if (eq? #f write-function) #!null write-function))
	domain)))
    ((_ var :: type value write-function)
     (define-property var :: type value write-function #!null))
    ((_ var :: type value)
     (define-property var :: type value #t #!null))
    ((_ var value write-function domain)
     (define-property var :: <Object> value write-function domain))
     ((_ var value write-function)
     (define-property var :: <Object> value write-function #!null))
     ((_ var value)
     (define-property var :: <Object> value #t #!null))
))


(define (set-globals rand sched sim) :: <void>
  (set! random rand)
  (set! schedule sched)
  (set! simulation sim))

(define (start-simulation) :: <void>
  (start))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (var times) body ...)
     (do ((var :: <int> 0 (+ var 1))) ((= var times)) body ...))))

(define-syntax dobag
  (syntax-rules (::)
    ((_ (elt bag) body ...)
     (let ((numobjs :: <int> bag:numObjs)
	   (objs :: <java.lang.Object[]> bag:objs))
       (do ((x :: <int> 0 (+ x 1))) ((= x numobjs)) 
	 (let ((elt (objs x)))
	   body ...))))
    ((_ (elt :: type bag) body ...)
     (let ((numobjs :: <int> bag:numObjs)
	   (objs :: <java.lang.Object[]> bag:objs))
       (do ((x :: <int> 0 (+ x 1))) ((= x numobjs)) 
	 (let ((elt :: type (objs x)))
	   body ...))))))

(define-simple-class <schemesimulation> (SimState)
  ((*init* seed :: <long>)
   (invoke-special SimState (this) '*init* seed)
   (set-globals random schedule (this)))
  
  ((start) :: <void>
   (invoke-special SimState (this) 'start)
   (set-globals random schedule (this))
   (start-simulation)))

(define controller :: Controller #!null) 
(define gui :: GUIState #!null)
(define displays '())

(define (init-gui c :: Controller) :: <void>
  (set! controller c)  ;; I do it this way to avoid using 'controller' in schemegui
  ;; since schemegui is a GUIState, which has 'controller' as an instance var279iable
  (init))

(define (setup-gui) :: <void>
  (setup)
  ;; reset the displays
  (map (lambda (display :: <sim.display.Display2D>)
	 (display:reset)
	 (display:repaint)) displays))

(define gui-label "")

;;;; It appears that I can't name this <scheme-gui>, as the name gets 
;;;; munged incorrectly by Kawa.
(define-simple-class <schemegui> (GUIState)
  ((*init*)
   (invoke-special GUIState (this) '*init*
		   (<schemesimulation> (System:currentTimeMillis)))
   (set! gui (this)))
  ((*init* state :: SimState)
   (invoke-special GUIState (this) '*init* state)
   (set! gui (this)))       
  
  ((getSimulationProperties) :: <sim.util.Properties>
   model-properties)

  ((getName) :: String
   allocation: 'static
   gui-label)
  
  ((init c :: Controller) :: <void>
   (invoke-special GUIState (this) 'init c)
   (init-gui c))
  
  ((start) :: <void>
   (invoke-special GUIState (this) 'start)
   (setup-gui))
  
  ((load state :: SimState) :: <void>
   (invoke-special GUIState (this) 'load state)
   (setup-gui)))

(define (go) :: <void>
  ((Console (<schemegui>)):setVisible #t))

(define (go-cl until) :: <void>
  (let ((sim (<schemesimulation> (System:currentTimeMillis))))
    (sim:start)
    (dotimes (u until)        ;;; presently we don't break if the simulation ran out of agents to schedule
	     (schedule:step sim))))

(define (make-display2d width height name backdrop-color)
  (let* ((display (Display2D width height gui 1))
	 (displayFrame (display:createFrame)))
    (display:setBackdrop backdrop-color)
    (displayFrame:setTitle (*:toString name))
    (controller:registerFrame displayFrame)
    (displayFrame:setVisible #t)
    (set! displays (cons display displays))
    display))


;;;; End library code



;;; Model global properties
(define-property minIdealTemp :: <double> 17000)
(define-property maxIdealTemp :: <double> 31000)
(define-property minOutputHeat :: <double> 6000)
(define-property maxOutputHeat :: <double> 10000)
(define-property evaporationRate :: <double> 0.993)
(define-property diffusionRate :: <double> 1.0)
(define-property randomMovementProbability :: <double> 0.1)
(define-property gridHeight :: <int>  100)
(define-property gridWidth :: <int> 100)
(define-property bugCount :: <int> 100)

;;; Model globals
(define valgrid :: DoubleGrid2D (DoubleGrid2D gridWidth gridHeight 0))
(define valgrid2 :: DoubleGrid2D (DoubleGrid2D gridWidth gridHeight 0))
(define buggrid :: SparseGrid2D (SparseGrid2D gridWidth gridHeight))
(define neighVal :: DoubleBag (DoubleBag))
(define neighX :: IntBag (IntBag))
(define neighY :: IntBag (IntBag))

;;; Constants
(define MAX-HEAT :: <double> 32000)

;;; GUI globals
(define display :: Display2D #!null)
(define flockersPortrayal :: ContinuousPortrayal2D (ContinuousPortrayal2D))            

;;; this statement sets the label for the Console window
(set! gui-label "Hexabugs")




(define-simple-class <hexabug> (Steppable)

  (idealTemp type: <double> init-form: 0)
  (heatOutput type: <double> init-form: 0)
  (maxHeat type: <double> init-form: 0)
  (randomMovementProbability type: <double> init-form: 0)

 
  ((*init* idealTemp- :: <double> heatOutput- :: <double>
	   maxHeat- :: <double> randomMovementProbability- :: <double>)
   (set! idealTemp idealTemp-)
   (set! heatOutput heatOutput-)
   (set! maxHeat maxHeat-)
   (set! randomMovementProbability randomMovementProbability-))

   ((addHeat grid :: DoubleGrid2D x :: <int> y :: <int> heat :: <double>) :: <void>
    (set! ((grid:field x) y) (+ ((grid:field x) y) heat))
    (if (> ((grid:field x) y) maxHeat)
	(set! ((grid:field x) y) maxHeat)))

   ((step state :: SimState)
    
    (let ((location :: Int2D (buggrid:getObjectLocation (this)))
	  (START :: <int> -1)
	  (bestx :: <int> -1)
	  (besty :: <int> 0))
      (let ((myx (location:getX))
	    (myy (location:getY)))

	(valgrid:getHexagonalNeighbors myx myy 1 valgrid:TOROIDAL #t neighVal neighX neighY)
	
	(if (random:nextBoolean randomMovementProbability)
	    (let ((temprandom :: <int> (random:nextInt neighX:numObjs)))
	      (set! bestx (neighX:objs temprandom))
	      (set! besty (neighY:objs temprandom)))
	    (if (> ((valgrid:field myx) myy) idealTemp)
		 (dotimes (i neighX:numObjs)
			  (if (or (/= (neighX:objs i) myx) 
				  (/= (neighY:objs i) myy))
			      (if (or (= bestx START)
				      (< (neighVal:objs i) ((valgrid:field bestx) besty))
				      (and (= (neighVal:objs i) ((valgrid:field bestx) besty))
					   (random:nextBoolean)))
				  (begin
				    (set! bestx (truncate (neighX:objs i)))
				    (set! besty (truncate (neighY:objs i)))))))
		(if (< ((valgrid:field myx) myy) idealTemp)
		     (dotimes (i neighX:numObjs)
			      (if (or (/= (neighX:objs i) myx) 
				      (/= (neighY:objs i) myy))
				  (if (or (= bestx START)
					  (> (neighVal:objs i) ((valgrid:field bestx) besty))
				    (and (> (neighVal:objs i) ((valgrid:field bestx) besty))   ;;; THIS MAKES NO SENSE  -- SEan
					 (random:nextBoolean)))
				(begin
				  (set! bestx (truncate (neighX:objs i)))
				  (set! besty (truncate (neighY:objs i)))))))
		     (begin
		       (set! bestx myx)
		       (set! besty myy)))))
	
	(buggrid:setObjectLocation (this) bestx besty)
	(addHeat valgrid bestx besty heatOutput)))))



(define-simple-class <diffuser> (Steppable)

  (updateGrid type: DoubleGrid2D init-form: #!null)
  (tempGrid type: DoubleGrid2D init-form: #!null)
  (evaporationRate type: <double> init-form: 0)
  (diffustionRate type: <double> init-form: 0)

  ((*init* updateGrid- :: DoubleGrid2D tempGrid- :: DoubleGrid2D
	   evaporationRate- :: <double> diffusionRate- :: <double>)
   (set! updateGrid updateGrid-)
   (set! tempGrid tempGrid-)
   (set! evaporationRate evaporationRate-)
   (set! diffusionRate diffusionRate-))

  ((step state :: SimState)
   (let ((-valgrid :: DoubleGrid2D updateGrid)
	 (-valgrid-field :: <double[][]> updateGrid:field)
	 (-valgrid2-field :: <double[][]> tempGrid:field))
	 
	 (let ((average :: <double> 0)
	       (-gridWidth :: <int> (-valgrid:getWidth))
	       (-gridHeight :: <int> (-valgrid:getHeight))
	       (-evaporationRate :: <double> evaporationRate)
	       (-diffusionRate :: <double> diffusionRate)
	       (-past :: <double[]> (-valgrid-field (-valgrid:stx -1)))
	       (-current :: <double[]> (-valgrid-field 0))
	       (-next :: <double[]> #!null)
	       (-put :: <double[]> #!null)
	       (yminus1 :: <int> 0)
	       (yplus1 :: <int> 0))

	   (dotimes (x -gridWidth)
		    (let ((xplus1 :: <int> (+ x 1))
			  (xmodulo2equals0 (= 0 (mod x 2))))
		      
		      (if (= xplus1 -gridWidth)
			  (set! xplus1 0))
		      (set! -next (-valgrid-field xplus1))
		      (set! -put (-valgrid2-field x))
		      (set! yminus1 (- -gridHeight 1))
		      
		      (dotimes (y -gridHeight)
			       (set! yplus1 (+ y 1))
			       (if (= yplus1 -gridHeight)
				   (set! yplus1 0))
			       (if xmodulo2equals0
				   (set! average 
					 (/ (+ (-current y)
					       (-past yminus1)
					       (-next yminus1)
					       (-past y)
					       (-next y)
					       (-current yminus1)
					       (-current yplus1))
					    7.0))
				   (set! average
					 (/ (+ (-current y)
					       (-past y)
					       (-next y)
					       (-past yplus1)
					       (-next yplus1)
					       (-current yminus1)
					       (-current yplus1))
					    7.0)))
			       (set! (-put y)
				     (* -evaporationRate (+ (-current y)
							    (* -diffusionRate (- average (-current y))))))

			       (set! yminus1 y))
		      
		      (set! -past -current)
		      (set! -current -next)))
	   
	   (updateGrid:setTo tempGrid)))))











(define-property bugs :: <hexabug[]> #!null)
(define-property diffuser :: <diffuser> #!null)








;; this function is called on the model when the user presses the play button,
;; so the model can set itself up

(define (start)
   (set! valgrid (DoubleGrid2D gridWidth gridHeight 0))
   (set! valgrid2 (DoubleGrid2D gridWidth gridHeight 0))
   (set! buggrid (SparseGrid2D gridWidth gridHeight))
   (set! bugs (<hexabug[]> length: bugCount))

   ;; schedule the bugs
   (dotimes (x bugCount)
	    (set! (bugs x) (<hexabug> (+ (* (random:nextDouble) (- maxIdealTemp minIdealTemp)) minIdealTemp)
				      (+ (* (random:nextDouble) (- maxOutputHeat minOutputHeat)) minOutputHeat)
				      MAX-HEAT randomMovementProbability))
	    (buggrid:setObjectLocation (bugs x) (random:nextInt gridWidth) (random:nextInt gridHeight))
	    (schedule:scheduleRepeating (bugs x)))
   
   ;; schedule the decreaser
   (set! diffuser (<diffuser> valgrid valgrid2 evaporationRate diffusionRate))

   (schedule:scheduleRepeating Schedule:EPOCH 1 diffuser 1))

(define (stop)
  (if (not (equal? #!null diffuser))
      (diffuser:cleanup))
  (set! diffuser #!null))

;; this function is called when the GUI is first being set up.

;(define (init)
;  (set! display (make-display2d 750 750 "Flockers" Color:black))
;  (display:attach flockersPortrayal "Behold the Scheme Flock!"))


;; Like 'start', this function is called on the model when the user pressed the
;; play button.  But it's also called when the user has loaded a model from disk.
;; In either case, your job here is to set up your portrayals to reflect the new
;; model.  I might work to merge this function with the above one if we don't care
;; about running on the command line any more, dunno.

;(define (setup)
;  (flockersPortrayal:setField field)
;  (dotimes (x field:allObjects:numObjs)
;	   (flockersPortrayal:setPortrayalForObject
;	    (field:allObjects:objs x)
;	    (OrientedPortrayal2D
;	     (SimplePortrayal2D)
;	     0 4.0 
;	     (Color 
;	      (+ 128 (random:nextInt 128))
;	      (+ 128 (random:nextInt 128))          
;	      (+ 128 (random:nextInt 128)))
;	     OrientedPortrayal2D:SHAPE_COMPASS))))



;; This function starts the simulation.


(go-cl 100000)