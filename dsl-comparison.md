# New Babylon DSL Comparison

This document compares the original JSON-style DSL syntax with the new Scheme-native syntax.

## Original JSON-style DSL vs Scheme-native DSL

The Babylonian Architecture Mapping DSL was originally implemented with a JSON-style syntax that required custom parsing. The new implementation leverages Scheme's natural ability to treat code as data, resulting in a simpler and more idiomatic approach.

### Project Definition

**Original JSON-style:**

```
Project "New Babylon Demo" {
    location: (34.5677, 40.1233)
    baseDate: 2025-04-20
    timeSpan: 50
    units: metric
}
```

**New Scheme-native:**

```scheme
(define-project "New Babylon Demo"
  (34.5677 40.1233)  ; location
  "2025-04-20"       ; base date
  50                 ; timespan
  'metric)           ; units
```

### Material Definition

**Original JSON-style:**

```
Material "Concrete" {
    density: 2400
    compressiveStrength: 30
    tensileStrength: 2.5
    weatherResistance: 0.85
    lifespan: 75
}
```

**New Scheme-native:**

```scheme
(define-babylon-material "Concrete"
  '(density . 2400)
  '(compressiveStrength . 30)
  '(tensileStrength . 2.5)
  '(weatherResistance . 0.85)
  '(lifespan . 75))
```

### Structure Definition

**Original JSON-style:**

```
Structure "MainTower" {
    geometry: Box {
        origin: (0, 0, 0)
        dimensions: (20, 20, 80)
    }
    material: @"Concrete"
    loadCapacity: 10.0
    connections: []
}
```

**New Scheme-native:**

```scheme
(define-babylon-structure "MainTower"
  (geometry box '(0 0 0) '(20 20 80))
  (material (reference 'material "Concrete"))
  (load-capacity 10.0)
  (connections))
```

### Space Definition

**Original JSON-style:**

```
Space "SkyGarden" {
    geometry: Polygon {
        points: [(5,5,40), (25,5,40), (25,15,40), (5,15,40)]
        height: 2
    }
    function: "urban-agriculture"
    capacity: 50
    socialAttributes: {
        interaction: "medium"
        education: "high"
        nutrition: "local"
    }
}
```

**New Scheme-native:**

```scheme
(define-babylon-space "SkyGarden"
  (geometry polygon '((5 5 40) (25 5 40) (25 15 40) (5 15 40)) 2.0)
  (function 'urban-agriculture)
  (capacity 50)
  (social-attributes
   '(interaction . medium)
   '(education . high)
   '(nutrition . local)))
```

### Connection Definition

**Original JSON-style:**

```
Connection "MainAccessBridge" {
    type: "pedestrian-bridge"
    elements: [@"MainTower", @"SkyGarden"]
    points: [
        {element: @"MainTower", position: (10,5,40)},
        {element: @"SkyGarden", position: (10,5,42)}
    ]
    structure: "standard-bridge"
    loadTransfer: {
        capacity: 30
    }
}
```

**New Scheme-native:**

```scheme
(define-babylon-connection "MainAccessBridge"
  (type "pedestrian-bridge")
  (elements
   (reference 'structure "MainTower")
   (reference 'space "SkyGarden"))
  (points
   '((10 5 40) . "MainTower")
   '((10 5 42) . "SkyGarden"))
  (structure "standard-bridge")
  (load-transfer
   '(capacity . 30)))
```

### Simulation Definition

**Original JSON-style:**

```
Simulate "StructuralAnalysis" {
    target: @"MainTower"
    conditions: 10.0
    analyze: ["load", "stability", "deflection"]
}
```

**New Scheme-native:**

```scheme
(babylon-simulate "StructuralAnalysis"
  (target (reference 'structure "MainTower"))
  (conditions 10.0)
  (analyze load stability deflection))
```

## Advantages of the Scheme-native DSL

1. **Simpler Implementation**: No need for complex custom parsing - Scheme already has a parser for S-expressions
2. **Better Integration**: Seamlessly integrates with the rest of the Scheme codebase
3. **Type Safety**: Better compile-time checking of the DSL syntax
4. **Extensibility**: Easier to extend with new features using Scheme's macro system
5. **IDE Support**: Better editor support with parenthesis matching and indentation
6. **Documentation**: Scheme's documentation tools work with the DSL
7. **Consistency**: The syntax follows the same rules as the rest of the codebase

## Implementation Comparison

The original JSON-style DSL required a complex tokenizer and parser (over 500 lines of code), while the Scheme-native DSL leverages Scheme's built-in reader and macro system, resulting in a much simpler implementation (less than 300 lines of code).

### Original Parsing Model

1. Tokenize the input text into tokens
2. Parse the tokens into a syntax tree
3. Interpret the syntax tree to create Goblins objects

### New Parsing Model

1. Use Scheme's reader to parse S-expressions directly
2. Use macros to interpret the S-expressions and create Goblins objects

This approach eliminates an entire layer of complexity and makes the DSL more maintainable and extensible.
