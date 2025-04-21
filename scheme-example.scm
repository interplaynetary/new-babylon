;; scheme-example.scm
;; Example usage of the Scheme-native Babylonian Architecture DSL

(use-modules (goblins)
             (goblins actor-lib cell)
             (goblins actor-lib let-on)
             (goblins actor-lib methods)
             (new-babylon)
             (new-babylon geometry)
             (new-babylon simulation)
             (new-babylon scheme-dsl))

;; =============================================
;; Define a complete Babylonian architecture model
;; =============================================

;; Define the project
(define-project "New Babylon Tower"
  '(34.5677 40.1233)  ; location
  "2025-04-20"       ; base date
  50                 ; timespan (years)
  'metric)           ; units

;; Define materials
(define-material "Concrete"
  '(density . 2400)
  '(compressiveStrength . 30)
  '(tensileStrength . 2.5)
  '(thermalExpansion . 0.000012)
  '(weatherResistance . 0.85)
  '(lifespan . 75))

(define-material "TensileMembranes"
  '(density . 1.5)
  '(tensileStrength . 3000)
  '(fireResistance . "class-B")
  '(transparency . 0.3)
  '(lifespan . 25))

;; Define structures
(define-structure "MainTower"
  (geometry box '(0 0 0) '(20 20 80))
  (material (-> 'material "Concrete"))
  (load-capacity 10.0)
  (connections))

(define-structure "ConnectorPlatform"
  (geometry polygon '((0 0 40) (30 0 40) (30 20 40) (0 20 40)) 0.5)
  (material (-> 'material "Concrete"))
  (load-capacity 5.0)
  (connections
   '((5 5 40) . "MainTower")
   '((25 5 40) . "ExistingBuilding")))

;; Define spaces
(define-space "SkyGarden"
  (geometry polygon '((5 5 40) (25 5 40) (25 15 40) (5 15 40)) 2.0)
  (function 'urban-agriculture)
  (capacity 50)
  (social-attributes
   '(interaction . medium)
   '(education . high)
   '(nutrition . local)))

(define-space "CommunityCenter"
  (geometry box '(5 5 50) '(20 10 3))
  (function 'gathering)
  (capacity 100)
  (social-attributes
   '(interaction . high)
   '(cultural . medium)
   '(visibility . 0.8)))

;; Define a connection
(define-connection "MainAccessBridge"
  (type "pedestrian-bridge")
  (elements 
   (-> 'structure "MainTower") 
   (-> 'space "SkyGarden"))
  (points 
   '((10 5 40) . "MainTower")
   '((10 5 42) . "SkyGarden"))
  (structure "standard-bridge")
  (load-transfer
   '(capacity . 30)))

;; Define an environment
(define-environment "MicroclimateStudy"
  (geometry box '(0 0 0) '(50 50 100))
  (wind
   '(direction . (1 0.5 0))
   '(speed . 5))
  (solar
   '(exposition . high))
  (thermal
   '(baseTemp . 22))
  (water
   '(drainage . "natural")))

;; Define an evolution
(define-evolution "GardenGrowth"
  (phase 0 
   '(occupancy . 0.3))
  (phase 2 
   '(extend . ((direction . "north") (distance . 10)))
   '(occupancy . 0.6))
  (phase 5 
   '(function . "market")
   '(capacity . 150)))

;; Define simulations and analyses
(simulate "StructuralAnalysis"
  (target (-> 'structure "MainTower"))
  (conditions 10.0)
  (analyze load stability deflection))

(query "SocialHotspots"
  (select "spaces")
  (from (-> 'space "SkyGarden"))
  (where "interaction > 0.5"))

(visualize "StructuralHealth"
  (target (-> 'structure "MainTower"))
  (mode "heatmap")
  (property "load-capacity-ratio"))

;; =============================================
;; Run a demo to show results
;; =============================================

(define (run-scheme-demo)
  (display "Running New Babylon Scheme DSL example...\n")
  
  ;; Access current project
  (let ((project (current-project)))
    
    ;; Show materials
    (display "Materials defined: ")
    (display (map (lambda (mat) ($ mat 'name))
                 ($ project 'list-elements 'material)))
    (newline)
    
    ;; Show structures
    (display "Structures defined: ")
    (display (map (lambda (struct) ($ struct 'name))
                 ($ project 'list-elements 'structure)))
    (newline)
    
    ;; Show spaces
    (display "Spaces defined: ")
    (display (map (lambda (space) ($ space 'name))
                 ($ project 'list-elements 'space)))
    (newline)
    
    ;; Run structural analysis
    (let* ((structural-sim ($ project 'get-element 'simulation "StructuralAnalysis"))
           (results ($ structural-sim 'run)))
      (display "Structural Analysis Results:\n")
      (for-each (lambda (result)
                  (display "  ")
                  (display result)
                  (newline))
                results))
    
    ;; Run query
    (let* ((query-obj ($ project 'get-element 'query "SocialHotspots"))
           (results ($ query-obj 'execute)))
      (display "Social Hotspots Query Results:\n")
      (display "  ")
      (display results)
      (newline))
    
    ;; Run visualization
    (let* ((viz ($ project 'get-element 'visualization "StructuralHealth"))
           (viz-result ($ viz 'render)))
      (display "Visualization Result:\n")
      (display "  ")
      (display viz-result)
      (newline))))

;; Example of how to load from a file
(define (load-babylon-model filename)
  (parameterize ((current-project #f))
    (load-babylon-file filename)))

;; Run the demo when this file is executed
(run-scheme-demo) 