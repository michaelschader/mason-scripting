;;;; Flockers in Scheme.
;;;; This experiment tries to whittle down a moderately complex simulation code (Flockers)
;;;; to a fairly simple-to-write Scheme code, and still be as fast as possible.
;;;; I have begun by writing a bunch of "library" code, which the user will never see,
;;;; which enables the user to write the Flocker code shown further on.  Ultimately this
;;;; library code, when elaborated and made more robust, will be put in a single file,
;;;; called, I dunno, mason.scm, which is loaded at the first line of the user's 
;;;; scheme program.

(require 'list-lib)

;;; Begin Library Code

(define-namespace Double2D <sim.util.Double2D>)
(define-namespace Continuous2D <sim.field.continuous.Continuous2D>)
(define-namespace SimState <sim.engine.SimState>)
(define-namespace MersenneTwisterFast <ec.util.MersenneTwisterFast>)
(define-namespace Steppable <sim.engine.Steppable>)
(define-namespace Oriented2D <sim.portrayal.Oriented2D>)
(define-namespace Bag <sim.util.Bag>)
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
  ;; since schemegui is a GUIState, which has 'controller' as an instance variable
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







;;;;;;;;;;;;;; Flocker Code
;;
;; My goals here were:
;;
;; 1. Use globals rather than instance variables for model features
;; 2. Eliminate classes and instead let the user just define functions.
;; 3. Create some common iterator macros to make the code simpler
;; 4. Eliminate as much as possible of the boilerplate that MASON requires users
;;    to type.
;; 5. Move many common MASON data types to simple names so the user can just
;;    refer to them that way.
;; 6. Move several standard MASON objects (the schedule, the console, etc.)
;;    to global variables so the user can just refer to them that way.
;;
;; I'm not done but I'm getting there.
;;
;;
;; What can't be done right now:
;;
;; 2. Classes named with hyphens in scheme style.  Seems to break.
;;
;; 
;; What hasn't been done yet:
;;
;; 1. A "def-agent" method.  Right now our flocker agents are still
;;    being done by defining, in scheme style, a MASON Steppable subclass.
;;    That's the next thing to clean up.
;;
;; 2. More cleanup of the GUI code boilerplate at the end
;;
;; 3. Perhaps merging of the GUI code for agents with the model code, so
;;    users don't need to think about separate portrayals and objects like
;;    they do in MASON.  It won't be as powerful but it'd be easier for the
;;    beginner to grok.

;; (define-agent NAME ... )
;; (new-agent NAME ... schedule-in: repeat: step: target:
;; expects the function FLOCKER-STEP which 
;; creates functions REMOVE-FLOCKER which deletes the flocker from the simulation
;; 


;; make a dummy agent used in the 'go' function
(define-simple-class referring-agent (Steppable)
  (target type: Steppable)
  (methodname type: symbol)
  ((*init* targ :: Steppable meth :: symbol)
   (set! target targ)
   (set! methodname meth))  
  ((step state :: SimState) :: <void>
   (target:(this):methodname)))

;; creates and schedules an agent to call a certain method on a given
;; target agent every so often perhaps
;(define (tryit target :: Steppable
;            method :: symbol
;            #!key
;            (at :: <double> (floor (+ 1.0 (schedule:getTime))))
;            (order :: <int> 0) 
;            (repeat 1.0))
;  (let ((agent (referring-agent target method)))
;    (if (eq? #f repeat)
;        (schedule:scheduleOnce at order agent)
;        (schedule:scheduleRepeating at order agent repeat))))

;; a simple superclass for agents so we can stick the orientation2D somewhere so it
;; can get overridden if necessary
(define-simple-class <superagent> (Steppable Oriented2D)
  (stopper type: <sim.engine.Stoppable> init-form: #!null)
  ((step state :: SimState) :: <void> ())
  ((orientation2D) :: <double>
   0.0)
  ((start . args) ())) ;; do nothing with start, but define it

;; create agents
(define-syntax define-agent
  (syntax-rules (::)
    ((_ agent-name method ...)
     (define-simple-class agent-name (<superagent>)
       ((*init*) ())  ;; don't do anything for the basic constructor.  We have to have this, it's a bug in Kawa I think
       ((step state :: SimState) :: <void>
        (method))  ;; call the method name
       ... ))))


;; schedules the agent and returns it
;(define (make agent-name
;          #!rest
;          args
;          #!key
;          (at :: <double> (floor (+ 1.0 (schedule:getTime))))
;          (order :: <int> 0)
;          (repeat 1.0))          ;; this will call (step) regardless of whether we want it to.  Should probabl;y fix that.
;  (let ((agent :: <superagent> (agent-name)))
;    (apply agent:start args)  ;; initialize
;    (if repeat
;        (schedule:scheduleRepeating at order agent repeat)
;        (schedule:scheduleOnce at order agent))
;    agent))

(define (stop agent :: <superagent>) :: <void>
  (if (not (eq? #!null agent:stopper))
      (begin
        (agent:stopper:stop)
        (set! agent:stopper #!null))))  ;; stopped... stop it no more, let the stopper GC



;; it's not necessary to have all these type declarations (<double>, Continuous2D, etc.),
;; but it makes things lots faster.

;;; Model globals
(define field :: Continuous2D #!null)

;;; Model global properties
(define-property width :: <double> 150)
(define-property height :: <double> 150)
(define-property numFlockers :: <int> 200)
(define-property cohesion-weight :: <double> 1.0)
(define-property avoidance-weight :: <double> 1.0)
(define-property randomness-weight :: <double> 1.0)
(define-property consistency-weight :: <double> 1.0)
(define-property momentum-weight :: <double>  1.0)
(define-property deadFlockerProbability :: <double> 0.1)
(define-property neighborhood :: <double> 10)
(define-property jump :: <double> 0.7)

;;; GUI globals
(define display :: Display2D #!null)
(define flockersPortrayal :: ContinuousPortrayal2D (ContinuousPortrayal2D))            

;;; this statement sets the label for the Console window
(set! gui-label "Scheme Flockers")



;; cannot use types recursively in a class declaration :-(

;(define-agent <flocker>
;; (define-simple-class <flocker> (Steppable Oriented2D)
;;     (loc type: Double2D init-form: (Double2D 0 0))
;;     (lastd type: Double2D init-form: (Double2D 0 0))
;;     (dead type: boolean init-form: #f)

;;  ((step state :: SimState) :: <void>
;;         (doit))

;;     ((*init* location :: Double2D)
;;      (set! loc location))

;;   ((getNeighbors) :: Bag
;;    (field:getObjectsExactlyWithinDistance loc neighborhood #t))

;;   ((getOrientation) :: <double>
;;    (orientation2D))

;;   ((isDead) :: boolean
;;    dead)

;;   ((setDead val :: boolean) :: <void>
;;    (set! dead val))

;;   ((orientation2D) :: <double>
;;    (if (and (= 0 lastd:x) (= 0 lastd:y)) 0 (atan lastd:y lastd:x)))

;;   ((consistency b :: Bag) :: Double2D
;;    (if (or (eq? b #!null) (= b:numObjs 0))
;;        (Double2D 0 0))
;;    (let ((x :: <double> 0)
;; 	 (y :: <double> 0)
;; 	 (count :: <int> 0))
;;      (dobag (other :: <flocker> b)
;; 	    (write other)
;; 	    (if (not (other:dead))
;; 		(let ((m :: Double2D other:lastd))
;; 		  (set! count (+ 1 count))
;; 		  (set! x (+ x m:x))
;; 		  (set! y (+ y m:y)))))
;;      (if (> count 0)
;; 	 (begin
;; 	   (set! x (/ x count))
;; 	   (set! y (/ y count))))
;;      (Double2D x y)))
  
;;   ((avoidance b :: Bag) :: Double2D
;;    (if (or (eq? b #!null) (= b:numObjs 0))
;;        (Double2D 0 0))
;;    (let ((x :: <double> 0)
;; 	 (y :: <double> 0)
;; 	 (count :: <int> 0))
;;      (dobag (other :: <flocker> b)
;; 	    (write other)
;; 	    (if (not (eq? other (this)))
;; 		(let* ((dx :: <double> (field:tdx loc:x other:loc:x))
;; 		       (dy :: <double> (field:tdy loc:y other:loc:y))
;; 		       (lensquared :: <double> (+ (* dx dx) (* dy dy))))
;; 		  (set! count (+ 1 count))
;; 		  (if (<= lensquared (* neighborhood neighborhood))
;; 		      (begin
;; 			(set! x (+ x (/ dx (+ (* lensquared lensquared) 1.0))))
;; 			(set! y (+ y (/ dy (+ (* lensquared lensquared) 1.0)))))))))
;;      (if (> count 0)
;; 	 (begin
;; 	   (set! x (/ x count))
;; 	   (set! y (/ y count))))
;;      (Double2D (* x 400) (* y 400))))
  
;;   ((cohesion b :: Bag) :: Double2D
;;    (if (or (eq? b #!null) (= b:numObjs 0))
;;        (Double2D 0 0))
;;    (let ((x :: <double> 0)
;; 	 (y :: <double> 0)
;;  	 (count :: <int> 0))
;;      (dobag (other :: <flocker> b)
;; 	    (write other)
;; 	    (if (not other:dead)
;; 		(let ((dx :: <double> (field:tdx loc:x other:loc:x))
;; 		      (dy :: <double> (field:tdy loc:y other:loc:y)))
;; 		  (set! count (+ 1 count))
;; 		  (set! x (+ x dx))
;; 		  (set! y (+ y dy)))))
;;      (if (> count 0)
;; 	 (begin 
;; 	   (set! x (/ x count))
;; 	   (set! y (/ y count))))
;;      (Double2D (/ x -10) (/ y -10))))
  
;;   ((randomness r :: MersenneTwisterFast) :: Double2D
;;    (let* ((x :: <double> (- (* (r:nextDouble) 2) 1.0))
;; 	  (y :: <double> (- (* (r:nextDouble) 2) 1.0))
;; 	  (l :: <double> (sqrt (+ (* x x) (* y y)))))
;;      (Double2D (* 0.05 (/ x l)) (* 0.05 (/ y l)))))

;;   ((doit) :: <void>
;;    (set! loc (field:getObjectLocation (this)))
;;    (if (not dead)
;;        (let* ((b :: Bag (getNeighbors))
;; 	      (avoid :: Double2D (avoidance b))
;; 	      (cohe :: Double2D (cohesion b))
;; 	      (rand :: Double2D (randomness random))
;; 	      (cons :: Double2D (consistency b))
;; 	      (mome :: Double2D lastd)
;; 	      (dx :: <double> (+ (* cohesion-weight cohe:x)
;; 				 (* avoidance-weight avoid:x)
;; 				 (* consistency-weight cons:x)
;; 				 (* randomness-weight rand:x)
;; 				 (* momentum-weight mome:x)))
;; 	      (dy :: <double> (+ (* cohesion-weight cohe:y)
;; 				 (* avoidance-weight avoid:y)
;; 				 (* consistency-weight cons:y)
;; 				 (* randomness-weight rand:y)
;; 				 (* momentum-weight mome:y)))
;; 	      (dis :: <double> (sqrt (+ (* dx dx) (* dy dy)))))
	      
;; 	 (if (> dis 0)
;; 	     (begin
;; 	       (set! dx (* (/ dx dis) jump))
;; 	       (set! dy (* (/ dy dis) jump))))
	 
;; 	 (set! lastd (Double2D dx dy))
;; 	 (set! loc (Double2D 
;; 		    (field:stx (+ loc:x dx)) 
;; 		    (field:sty (+ loc:y dy))))
;; 	 (field:setObjectLocation (this) loc))))
;;   )


(define-simple-class <flocker> (Steppable Oriented2D)

    (loc type: Double2D init-form: (Double2D 0 0))
    (lastd type: Double2D init-form: (Double2D 0 0))
    (dead type: <boolean> init-form: #f)
  
  ((getNeighbors) :: Bag
   (field:getObjectsExactlyWithinDistance loc neighborhood #t))

  ((getOrientation) :: double
   (orientation2D))

  ((isDead) :: <boolean>
   dead)

  ((setDead val :: <boolean>) :: <void>
   (set! dead val))

  ((orientation2D) :: double
   (if (and (= 0 lastd:x) (= 0 lastd:y)) 0 (atan lastd:y lastd:x)))

  ((*init* location :: Double2D)
   (set! loc location))       
  
  ((consistency b :: Bag) :: Double2D
   (if (or (eq? b #!null) (= b:numObjs 0))
       (Double2D 0 0))
   (let ((x :: double 0)
         (y :: double 0)
         (count :: int 0))
     (dobag (other :: <flocker> b)
            (if (not other:dead)
                (let ((m :: Double2D other:lastd))
                  (set! count (+ 1 count))
                  (set! x (+ x m:x))
                  (set! y (+ y m:y)))))
     (if (> count 0)
         (begin
           (set! x (/ x count))
           (set! y (/ y count))))
     (Double2D x y)))
  
  ((avoidance b :: Bag) :: Double2D
   (if (or (eq? b #!null) (= b:numObjs 0))
       (Double2D 0 0))
   (let ((x :: double 0)
         (y :: double 0)
         (count :: int 0))
     (dobag (other :: <flocker> b)
            (if (not (eq? other (this)))
                (let* ((dx :: double (field:tdx loc:x other:loc:x))
                       (dy :: double (field:tdy loc:y other:loc:y))
                       (lensquared :: double (+ (* dx dx) (* dy dy))))
                  (set! count (+ 1 count))
                  (if (<= lensquared (* neighborhood neighborhood))
                      (begin
                        (set! x (+ x (/ dx (+ (* lensquared lensquared) 1.0))))
                        (set! y (+ y (/ dy (+ (* lensquared lensquared) 1.0)))))))))
     (if (> count 0)
         (begin
           (set! x (/ x count))
           (set! y (/ y count))))
     (Double2D (* x 400) (* y 400))))
  
  ((cohesion b :: Bag) :: Double2D
   (if (or (eq? b #!null) (= b:numObjs 0))
       (Double2D 0 0))
   (let ((x :: double 0)
         (y :: double 0)
         (count :: int 0))
     (dobag (other :: <flocker> b)
            (if (not other:dead)
                (let ((dx :: double (field:tdx loc:x other:loc:x))
                      (dy :: double (field:tdy loc:y other:loc:y)))
                  (set! count (+ 1 count))
                  (set! x (+ x dx))
                  (set! y (+ y dy)))))
     (if (> count 0)
         (begin 
           (set! x (/ x count))
           (set! y (/ y count))))
     (Double2D (/ x -10) (/ y -10))))
  
  ((randomness r :: MersenneTwisterFast) :: Double2D
   (let* ((x :: double (- (* (r:nextDouble) 2) 1.0))
          (y :: double (- (* (r:nextDouble) 2) 1.0))
          (l :: double (sqrt (+ (* x x) (* y y)))))
     (Double2D (* 0.05 (/ x l)) (* 0.05 (/ y l)))))

  ((step state :: SimState) :: <void>
   (set! loc (field:getObjectLocation (this)))
   (if (not dead)
       (let* ((b :: Bag (getNeighbors))
              (avoid :: Double2D (avoidance b))
              (cohe :: Double2D (cohesion b))
              (rand :: Double2D (randomness random))
              (cons :: Double2D (consistency b))
              (mome :: Double2D lastd)
              (dx :: double (+ (* cohesion-weight cohe:x)
                                 (* avoidance-weight avoid:x)
                                 (* consistency-weight cons:x)
                                 (* randomness-weight rand:x)
                                 (* momentum-weight mome:x)))
              (dy :: double (+ (* cohesion-weight cohe:y)
                                 (* avoidance-weight avoid:y)
                                 (* consistency-weight cons:y)
                                 (* randomness-weight rand:y)
                                 (* momentum-weight mome:y)))
              (dis :: double (sqrt (+ (* dx dx) (* dy dy)))))
              
         (if (> dis 0)
             (begin
               (set! dx (* (/ dx dis) jump))
               (set! dy (* (/ dy dis) jump))))
         
         (set! lastd (Double2D dx dy))
         (set! loc (Double2D 
                    (field:stx (+ loc:x dx)) 
                    (field:sty (+ loc:y dy))))
         (field:setObjectLocation (this) loc)))))






;; this function is called on the model when the user presses the play button,
;; so the model can set itself up

(define (start)
  (set! field (Continuous2D (/ neighborhood 1.5) width height))
  (dotimes (x numFlockers)
	   (let* ((location (Double2D 
			     (* (random:nextDouble) width)
			     (* (random:nextDouble) height)))
		  (flocker (<flocker> location)))
	     (if (random:nextBoolean deadFlockerProbability)
		 (set! flocker:dead #t))
	     (field:setObjectLocation flocker location)
	     (schedule:scheduleRepeating flocker))))



;; this function is called when the GUI is first being set up.

(define (init)
  (set! display (make-display2d 750 750 "Flockers" Color:black))
  (display:attach flockersPortrayal "Behold the Scheme Flock!"))


;; Like 'start', this function is called on the model when the user pressed the
;; play button.  But it's also called when the user has loaded a model from disk.
;; In either case, your job here is to set up your portrayals to reflect the new
;; model.  I might work to merge this function with the above one if we don't care
;; about running on the command line any more, dunno.

(define (setup)
  (flockersPortrayal:setField field)
  (dotimes (x field:allObjects:numObjs)
	   (flockersPortrayal:setPortrayalForObject
	    (field:allObjects:objs x)
	    (OrientedPortrayal2D
	     (SimplePortrayal2D)
	     0 4.0 
	     (Color 
	      (+ 128 (random:nextInt 128))
	      (+ 128 (random:nextInt 128))          
	      (+ 128 (random:nextInt 128)))
	     OrientedPortrayal2D:SHAPE_COMPASS))))

;; I got rid of this code for the moment.  It's not really crucial.
;;  (let ((w (field:getWidth))
;;	(h (field:getHeight)))
;;    (if (= w h)
;;	(begin (set! display:insideDisplay:width 750)
;;	       (set! display:insideDisplay:height 750))
;;	(if (> w h)
;;	    (begin (set! display:insideDisplay:width (* 750 (/ h w)))
;;		   (set! display:insideDisplay:height (* 750 (/ h w))))
;;	    (if (< w h)
;;		(begin (set! display:insideDisplay:width (* 750 (/ w h)))
;;		       (set! display:insideDisplay:height (* 750 (/ w h))))))))


;; This function starts the simulation.
(go)
