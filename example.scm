;; example.scm
;; Example usage of the New Babylon DSL with Goblins

(use-modules (goblins)
             (goblins actor-lib cell)
             (goblins actor-lib let-on)
             (goblins actor-lib methods)
             (new-babylon)
             (new-babylon geometry)
             (new-babylon simulation)
             (new-babylon dsl))

;; =========================================
;; Manual API usage example
;; =========================================

;; Create a new project
(define demo-project 
  (make-project "New Babylon Demo" 
                '(34.5677 40.1233) 
                "2025-04-20" 
                50 
                'metric))

;; Define materials
(define concrete 
  (define-material demo-project "LightweightConcrete"
    `((density . 1800)
      (compressiveStrength . 25)
      (tensileStrength . 2.0)
      (thermalExpansion . 0.000010)
      (weatherResistance . 0.8)
      (lifespan . 60))))

(define membrane 
  (define-material demo-project "TensileMembranes"
    `((density . 1.5)
      (tensileStrength . 3000)
      (fireResistance . "class-B")
      (transparency . 0.3)
      (lifespan . 25))))

;; Define a structure
(define platform-geometry 
  (make-polygon '((0 0 15) (30 0 15) (30 20 15) (0 20 15)) 0.5))

(define platform 
  (define-structure demo-project "ElevatedPlatform"
    platform-geometry
    concrete
    '(5.0)
    '(((5 5 0) . "ExistingBuilding-A")
      ((25 5 0) . "ExistingBuilding-B")
      ((15 15 0) . "new-column"))))

;; Define a space
(define garden-geometry 
  (make-polygon '((5 5 15) (25 5 15) (25 15 15) (5 15 15)) 2.0))

(define garden 
  (define-space demo-project "CommunityGarden"
    garden-geometry
    'urban-agriculture
    50
    `((interaction . medium)
      (education . high)
      (nutrition . local))))

;; Create a connection
(define bridge 
  (define-connection demo-project "MainAccessBridge"
    "pedestrian-bridge"
    (list platform garden)
    '(((10 5 15) . "ElevatedPlatform")
      ((10 5 17) . "CommunityGarden"))
    'standard-bridge
    '((capacity . 30))))

;; Define an environment
(define env-geometry 
  (make-box '(0 0 0) '(50 50 30)))

(define environment 
  (define-environment demo-project "MicroclimateStudy"
    env-geometry
    '((direction . (1 0.5 0))
      (speed . 5))
    '((exposition . high))
    '((baseTemp . 22))
    '((drainage . "natural"))))

;; Define evolution
(define evolution 
  (define-evolution demo-project "GardenGrowth"
    '((0 ((occupancy . 0.3)))
      (2 ((extend . ((direction . "north")
                    (distance . 10)))
          (occupancy . 0.6)))
      (5 ((function . "market")
          (capacity . 150))))))

;; Run a simulation
(define structural-sim 
  (simulate demo-project "StructuralAnalysis"
    platform
    '(10.0)
    '(load stability deflection)))

;; Run queries
(define social-query 
  (query demo-project "SocialHotspots"
    "spaces"
    garden
    "interaction > 0.5"))

;; Example of visualization
(define viz 
  (visualize demo-project "StructuralHealth"
    platform
    "heatmap"
    "load-capacity-ratio"))

;; =========================================
;; DSL example - equivalent to above code
;; =========================================

(define babylon-dsl-example "
Project \"New Babylon Demo\" {
    location: (34.5677, 40.1233)
    baseDate: 2025-04-20
    timeSpan: 50
    units: metric
}

Material \"LightweightConcrete\" {
    density: 1800
    compressiveStrength: 25
    tensileStrength: 2.0
    thermalExpansion: 0.000010
    weatherResistance: 0.8
    lifespan: 60
}

Material \"TensileMembranes\" {
    density: 1.5
    tensileStrength: 3000
    fireResistance: \"class-B\"
    transparency: 0.3
    lifespan: 25
}

Structure \"ElevatedPlatform\" {
    geometry: Polygon {
        points: [(0,0,15), (30,0,15), (30,20,15), (0,20,15)]
        height: 0.5
    }
    material: @\"LightweightConcrete\"
    loadCapacity: 5.0
    connections: [
        {location: (5,5,0), connection: \"ExistingBuilding-A\"},
        {location: (25,5,0), connection: \"ExistingBuilding-B\"},
        {location: (15,15,0), type: \"new-column\"}
    ]
}

Space \"CommunityGarden\" {
    geometry: Polygon {
        points: [(5,5,15), (25,5,15), (25,15,15), (5,15,15)]
        height: 2
    }
    function: \"urban-agriculture\"
    capacity: 50
    socialAttributes: {
        interaction: \"medium\"
        education: \"high\"
        nutrition: \"local\"
    }
}

Connection \"MainAccessBridge\" {
    type: \"pedestrian-bridge\"
    elements: [@\"ElevatedPlatform\", @\"CommunityGarden\"]
    points: [
        {element: @\"ElevatedPlatform\", position: (10,5,15)},
        {element: @\"CommunityGarden\", position: (10,5,17)}
    ]
    structure: \"standard-bridge\"
    loadTransfer: {
        capacity: 30
    }
}

Environment \"MicroclimateStudy\" {
    geometry: Box {
        origin: (0,0,0)
        dimensions: (50,50,30)
    }
    wind: {
        direction: (1,0.5,0)
        speed: 5
    }
    solar: {
        exposition: \"high\"
    }
    thermal: {
        baseTemp: 22
    }
    water: {
        drainage: \"natural\"
    }
}

Evolution \"GardenGrowth\" {
    phase(0) {
        occupancy: 0.3
    }
    phase(2) {
        extend: {
            direction: \"north\"
            distance: 10
        }
        occupancy: 0.6
    }
    phase(5) {
        function: \"market\"
        capacity: 150
    }
}

Simulate \"StructuralAnalysis\" {
    target: @\"ElevatedPlatform\"
    conditions: 10.0
    analyze: [\"load\", \"stability\", \"deflection\"]
}

Query \"SocialHotspots\" {
    select: \"spaces\"
    from: @\"CommunityGarden\"
    where: \"interaction > 0.5\"
}

Visualize \"StructuralHealth\" {
    target: @\"ElevatedPlatform\"
    mode: \"heatmap\"
    property: \"load-capacity-ratio\"
}
")

;; Parse the DSL example
(define dsl-project (parse-dsl babylon-dsl-example))

;; =========================================
;; Running the example
;; =========================================

(define (run-demo)
  (display "Running New Babylon manual API example...\n")
  
  ;; Run structural analysis
  (let ((results ($ structural-sim 'run)))
    (display "Structural Analysis Results:\n")
    (for-each (lambda (result)
                (display "  ")
                (display result)
                (newline))
              results))
  
  ;; Query social hotspots
  (let ((results ($ social-query 'execute)))
    (display "Social Hotspots Query Results:\n")
    (display "  ")
    (display results)
    (newline))
  
  ;; Generate visualization
  (let ((viz-result ($ viz 'render)))
    (display "Visualization Result:\n")
    (display "  ")
    (display viz-result)
    (newline))
  
  (display "\nRunning New Babylon DSL example...\n")
  (display "Project parsed successfully: ")
  (display ($ dsl-project 'name))
  (newline)
  
  (display "Materials defined: ")
  (display (map (lambda (mat) ($ mat 'name))
               ($ dsl-project 'list-elements 'material)))
  (newline)
  
  (display "Structures defined: ")
  (display (map (lambda (struct) ($ struct 'name))
               ($ dsl-project 'list-elements 'structure)))
  (newline))

;; Run the demo when this file is executed
(run-demo) 