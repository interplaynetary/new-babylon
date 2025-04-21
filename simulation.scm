;; simulation.scm
;; Simulation capabilities for the New Babylon DSL

(define-module (new-babylon simulation)
  #:use-module (goblins)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib let-on)
  #:use-module (goblins actor-lib cell)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (^structural-simulator
            ^social-simulator
            ^environmental-simulator
            ^temporal-simulator
            make-structural-simulator
            make-social-simulator
            make-environmental-simulator
            make-temporal-simulator))

;; ==========================================
;; Simulation actors constructors
;; ==========================================

;; Structural simulator
(define (^structural-simulator bcom)
  (methods
   ((analyze-load structure load-conditions)
    (let-on ((name (<- structure 'name))
             (load-capacity (<- structure 'load-capacity)))
      ;; Simplified structural analysis
      (let ((stress-ratio (/ (car load-conditions) 
                            (car load-capacity))))
        `((structure ,name)
          (load-ratio ,stress-ratio)
          (critical ,(> stress-ratio 0.8))))))
   
   ((analyze-stability structures connections conditions)
    ;; Placeholder for stability analysis of multiple connected structures
    (let* ((structures-number (length structures))
           (connections-number (length connections))
           (stability-factor (/ connections-number 
                               (max 1 (- structures-number 1)))))
      `((structures-count ,structures-number)
        (connections-count ,connections-number)
        (stability-factor ,stability-factor)
        (stable ,(>= stability-factor 1.0)))))
   
   ((analyze-deflection structure load-conditions)
    ;; Simplified deflection analysis
    (let-on ((name (<- structure 'name))
             (material (<- structure 'material)))
      (let-on ((elasticity (<- material 'get-property 'elasticity)))
        ;; Default elasticity if not defined
        (let ((e (or elasticity 200000)))
          (let ((load (car load-conditions))
                (length 5.0)) ; Simplified span
            (let ((deflection (* 5 (/ (* load (* length length length length))
                                     (* 384 e 1000)))))
              `((structure ,name)
                (deflection ,deflection)
                (acceptable ,(< deflection 0.01)))))))))
   
   ((run-structural-analysis target conditions analysis-types)
    ;; Run all requested analysis types
    (map
     (lambda (analysis-type)
       (case analysis-type
         ((load) ($ bcom 'analyze-load target conditions))
         ((stability) ($ bcom 'analyze-stability (list target) '() conditions))
         ((deflection) ($ bcom 'analyze-deflection target conditions))
         (else `(unknown-analysis-type ,analysis-type))))
     analysis-types))))

;; Social simulator
(define (^social-simulator bcom)
  (methods
   ((analyze-occupancy space scenario)
    (let-on ((name (<- space 'name))
             (capacity (<- space 'capacity))
             (function (<- space 'function)))
      ;; Simplified occupancy analysis based on function and scenario
      (let* ((base-occupancy (case function
                              ((gathering) 0.7)
                              ((circulation) 0.3)
                              ((residential) 0.5)
                              ((commercial) 0.6)
                              (else 0.4)))
             ;; Adjust based on scenario
             (scenario-factor (case scenario
                               ((peak) 1.5)
                               ((weekend) 1.2)
                               ((weekday) 0.8)
                               ((night) 0.3)
                               (else 1.0)))
             (occupancy (* base-occupancy scenario-factor))
             ;; Cap at 1.0 maximum
             (final-occupancy (min 1.0 occupancy))
             (people-count (floor (* capacity final-occupancy))))
        `((space ,name)
          (capacity ,capacity)
          (scenario ,scenario)
          (occupancy-ratio ,final-occupancy)
          (people-count ,people-count)))))
   
   ((analyze-interaction spaces connections scenario)
    ;; Analyze social interaction between connected spaces
    (let* ((space-count (length spaces))
           (connection-count (length connections))
           ;; More connections encourage interaction
           (base-interaction (min 1.0 (/ (* 2 connection-count) space-count)))
           ;; Adjust based on scenario
           (scenario-factor (case scenario
                             ((festival) 1.5)
                             ((event) 1.3)
                             ((normal) 1.0)
                             ((quiet) 0.7)
                             (else 1.0)))
           (interaction-level (* base-interaction scenario-factor)))
      `((spaces ,space-count)
        (connections ,connection-count)
        (interaction-level ,interaction-level))))
   
   ((analyze-circulation space connections scenario)
    (let-on ((name (<- space 'name)))
      ;; Simplified circulation analysis
      (let* ((entry-points (length connections))
             (circulation-factor (min 1.0 (/ entry-points 2)))
             (scenario-modifier (case scenario
                                 ((peak) 0.7) ; congested
                                 ((normal) 1.0)
                                 ((low) 1.3) ; free-flowing
                                 (else 1.0)))
             (flow-quality (* circulation-factor scenario-modifier)))
        `((space ,name)
          (entry-points ,entry-points)
          (flow-quality ,flow-quality)))))
   
   ((run-social-analysis target scenario analysis-types)
    ;; Run all requested analysis types
    (map
     (lambda (analysis-type)
       (case analysis-type
         ((occupancy) ($ bcom 'analyze-occupancy target scenario))
         ((interaction) ($ bcom 'analyze-interaction (list target) '() scenario))
         ((circulation) ($ bcom 'analyze-circulation target '() scenario))
         (else `(unknown-analysis-type ,analysis-type))))
     analysis-types))))

;; Environmental simulator
(define (^environmental-simulator bcom)
  (methods
   ((analyze-solar target time-of-day time-of-year)
    ;; Simplified solar analysis
    (let-on ((name (<- target 'name))
             (geometry (<- target 'geometry)))
      (let* ((hour (cond
                    ((eq? time-of-day 'morning) 9)
                    ((eq? time-of-day 'noon) 12)
                    ((eq? time-of-day 'afternoon) 15)
                    ((eq? time-of-day 'evening) 18)
                    (else 12)))
             (season (cond
                      ((eq? time-of-year 'summer) 'summer)
                      ((eq? time-of-year 'winter) 'winter)
                      ((eq? time-of-year 'spring) 'spring)
                      ((eq? time-of-year 'fall) 'fall)
                      (else 'summer)))
             (base-intensity (cond
                              ((eq? season 'summer) 1.0)
                              ((eq? season 'winter) 0.5)
                              (else 0.75)))
             (time-factor (cond
                           ((= hour 12) 1.0)
                           ((or (= hour 9) (= hour 15)) 0.8)
                           ((= hour 18) 0.4)
                           (else 0.6)))
             (solar-intensity (* base-intensity time-factor)))
        `((target ,name)
          (time-of-day ,time-of-day)
          (season ,season)
          (solar-intensity ,solar-intensity)))))
   
   ((analyze-wind target wind-conditions)
    ;; Simplified wind analysis
    (let-on ((name (<- target 'name))
             (geometry (<- target 'geometry)))
      (let* ((wind-direction (car wind-conditions))
             (wind-speed (cadr wind-conditions))
             ;; Placeholder for actual analysis
             (wind-factor (* wind-speed 0.1))
             (wind-comfort (cond
                           ((< wind-speed 2) 'stagnant)
                           ((< wind-speed 5) 'comfortable)
                           ((< wind-speed 10) 'breezy)
                           (else 'uncomfortable))))
        `((target ,name)
          (wind-direction ,wind-direction)
          (wind-speed ,wind-speed)
          (wind-comfort ,wind-comfort)))))
   
   ((analyze-thermal target weather-conditions)
    ;; Simplified thermal comfort analysis
    (let-on ((name (<- target 'name)))
      (let* ((ambient-temp (car weather-conditions))
             (humidity (cadr weather-conditions))
             ;; Very simplified approximation of comfort
             (discomfort-factor (+ (abs (- ambient-temp 22))
                                  (* (/ humidity 100) 10)))
             (comfort-level (cond
                            ((< discomfort-factor 5) 'comfortable)
                            ((< discomfort-factor 10) 'acceptable)
                            ((< discomfort-factor 15) 'uncomfortable)
                            (else 'extreme))))
        `((target ,name)
          (temperature ,ambient-temp)
          (humidity ,humidity)
          (discomfort-factor ,discomfort-factor)
          (comfort-level ,comfort-level)))))
   
   ((run-environmental-analysis target conditions analysis-types)
    ;; Run all requested analysis types
    (map
     (lambda (analysis-type)
       (case analysis-type
         ((solar) ($ bcom 'analyze-solar target 
                     (assoc-ref conditions 'time-of-day)
                     (assoc-ref conditions 'time-of-year)))
         ((wind) ($ bcom 'analyze-wind target 
                    (assoc-ref conditions 'wind)))
         ((thermal) ($ bcom 'analyze-thermal target 
                       (assoc-ref conditions 'weather)))
         (else `(unknown-analysis-type ,analysis-type))))
     analysis-types))))

;; Temporal simulator for evolution over time
(define (^temporal-simulator bcom)
  (methods
   ((project-state element base-state time-point)
    ;; Project how element will change over time
    (let-on ((name (<- element 'name))
             (evolutions (<- element 'get-related 'evolution)))
      ;; Simplified - just apply the closest evolution phase
      (if (null? evolutions)
          `((element ,name)
            (time ,time-point)
            (state ,base-state)
            (changes none))
          (let-on ((phases (<- (car evolutions) 'phases)))
            ;; Find the closest phase before the time-point
            (let ((applicable-phase (find (lambda (p)
                                           (<= (car p) time-point))
                                         phases)))
              (if applicable-phase
                  `((element ,name)
                    (time ,time-point)
                    (state ,(cadr applicable-phase))
                    (phase-time ,(car applicable-phase)))
                  `((element ,name)
                    (time ,time-point)
                    (state ,base-state)
                    (changes none))))))))
   
   ((analyze-material-aging material time-point)
    (let-on ((name (<- material 'name))
             (lifespan (<- material 'get-property 'lifespan 50)))
      ;; Simplified aging model
      (let* ((age-ratio (/ time-point lifespan))
             (condition (cond
                        ((< age-ratio 0.3) 'excellent)
                        ((< age-ratio 0.6) 'good)
                        ((< age-ratio 0.8) 'fair)
                        ((< age-ratio 1.0) 'poor)
                        (else 'critical))))
        `((material ,name)
          (age-ratio ,age-ratio)
          (condition ,condition)))))
   
   ((analyze-usage-patterns space time-span interval)
    (let-on ((name (<- space 'name))
             (function (<- space 'function))))
      ;; Simplified usage patterns
      (let* ((hourly-pattern (case function
                              ((residential) '(0.8 0.8 0.8 0.8 0.8 0.9 1.0 0.9 
                                             0.5 0.3 0.2 0.3 0.5 0.4 0.4 0.6 
                                             0.8 0.9 0.9 0.9 0.9 0.9 0.8 0.8))
                              ((commercial) '(0.0 0.0 0.0 0.0 0.0 0.1 0.2 0.5 
                                            0.8 0.9 1.0 1.0 1.0 0.9 0.9 0.9 
                                            0.8 0.6 0.4 0.2 0.1 0.0 0.0 0.0))
                              ((gathering) '(0.1 0.0 0.0 0.0 0.0 0.0 0.0 0.1 
                                           0.2 0.3 0.5 0.8 1.0 0.9 0.7 0.6 
                                           0.7 0.8 0.9 0.8 0.6 0.4 0.2 0.1))
                              (else '(0.4 0.3 0.2 0.2 0.2 0.3 0.5 0.7 
                                     0.8 0.9 0.9 0.8 0.8 0.8 0.8 0.8 
                                     0.8 0.8 0.7 0.6 0.5 0.4 0.4 0.4))))
             ;; Simplistic - just return hourly pattern
             (time-series (map (lambda (hour)
                                `(,(* hour interval) ,(list-ref hourly-pattern 
                                                             (modulo hour 24))))
                              (iota (/ time-span interval)))))
        `((space ,name)
          (function ,function)
          (time-span ,time-span)
          (interval ,interval)
          (usage-pattern ,time-series))))
   
   ((run-temporal-analysis target time-span intervals analysis-types)
    ;; Run all requested analysis types
    (map
     (lambda (analysis-type)
       (case analysis-type
         ((state-projection) ($ bcom 'project-state target 'initial time-span))
         ((material-aging) (let-on ((material (<- target 'material)))
                            ($ bcom 'analyze-material-aging material time-span)))
         ((usage-patterns) ($ bcom 'analyze-usage-patterns target time-span intervals))
         (else `(unknown-analysis-type ,analysis-type))))
     analysis-types))))

;; ==========================================
;; Public API functions
;; ==========================================

;; Create simulators
(define (make-structural-simulator)
  (spawn ^structural-simulator (spawn-bcom)))

(define (make-social-simulator)
  (spawn ^social-simulator (spawn-bcom)))

(define (make-environmental-simulator)
  (spawn ^environmental-simulator (spawn-bcom)))

(define (make-temporal-simulator)
  (spawn ^temporal-simulator (spawn-bcom))) 