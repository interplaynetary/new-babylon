# new-babylon

Protomaps: openstreetmap tools for new babylonian architecture
https://protomaps.com/

# Babylonian Architecture Mapping Library & DSL
## Project Overview

The Babylonian Architecture Mapping Library (BAML) is an innovative software framework designed to enable architects, urban planners, and developers to create new architectural configurations that intelligently integrate with existing urban fabric. By leveraging a custom Domain-Specific Language (BAMDSL), the system provides comprehensive 4D modeling capabilities that account for structural, social, environmental, and temporal dimensions of architectural interventions.

## Core Purpose

BAML addresses the emerging challenge of creating new "Babylonian" architectural forms—complex, layered structures that integrate with and build upon existing architecture. Rather than demolishing and rebuilding, this approach allows for adaptive reuse, vertical expansion, and interconnection of structures across space and time. The system enables users to:

1. Map existing architectural properties in comprehensive detail
2. Simulate the impact of new structural additions and modifications
3. Analyze both physical and social implications of architectural interventions
4. Model changes and adaptations over time (4D)
5. Generate detailed visualizations and reports for decision-making

## System Architecture

The BAML system consists of several integrated components:

### 1. Core Mapping Engine
- Imports and processes existing architectural data (GIS, BIM, etc.)
- Creates comprehensive 3D spatial models with structural properties
- Maintains a relational database of architectural elements and their characteristics

### 2. Domain-Specific Language (BAMDSL)
- Provides a specialized syntax for defining architectural configurations
- Enables precise specification of properties, relationships, and behaviors
- Supports temporal modeling and evolutionary scenarios

### 3. Simulation Framework
- Performs structural analysis, load calculations, and stress testing
- Models social flows, interactions, and space utilization
- Simulates environmental factors and temporal evolution

### 4. Visualization Layer
- Generates 3D/4D representations of architectural configurations
- Provides data visualization for various property types
- Supports interactive exploration and presentation

## Core Grammar of BAMDSL

The BAMDSL language features a consistent, hierarchical syntax designed for clarity and expressiveness:

### Basic Structure
```
Element "IdentifierName" {
    property: value
    nestedProperty: {
        subProperty: value
    }
    arrayProperty: [value1, value2]
    reference: @"ReferencedElement"
}
```

### Primary Elements

1. **Project** - Defines the overall scope, location, and parameters
   ```
   Project "ProjectName" {
       location: (coordinates)
       baseDate: date
       timeSpan: duration
       units: unitSystem
   }
   ```

2. **Material** - Defines physical properties of construction materials
   ```
   Material "MaterialName" {
       density: value
       strength: value
       lifespan: duration
       // Additional physical properties
   }
   ```

3. **Structure** - Defines architectural elements with load-bearing properties
   ```
   Structure "StructureName" {
       geometry: GeometryType { /* parameters */ }
       material: @"MaterialReference"
       loadCapacity: { /* load specifications */ }
       connections: [ /* connection points */ ]
   }
   ```

4. **Space** - Defines functional areas with social properties
   ```
   Space "SpaceName" {
       geometry: GeometryType { /* parameters */ }
       function: "functionType"
       capacity: value
       socialAttributes: { /* social parameters */ }
   }
   ```

5. **Connection** - Defines relationships between elements
   ```
   Connection "ConnectionName" {
       type: "connectionType"
       elements: [@"Element1", @"Element2"]
       // Additional properties
   }
   ```

6. **Environment** - Defines environmental context and factors
   ```
   Environment "EnvironmentName" {
       geometry: GeometryType { /* parameters */ }
       // Environmental factors (wind, solar, etc.)
   }
   ```

7. **System** - Defines infrastructure networks and services
   ```
   System "SystemName" {
       type: "systemType"
       nodes: [ /* distribution nodes */ ]
       connections: [ /* system connections */ ]
   }
   ```

8. **Evolution** - Defines temporal changes and phases
   ```
   Evolution "EvolutionName" {
       phase(timePoint) {
           // Modifications at this time point
       }
       phase(laterTimePoint) {
           // Later modifications
       }
   }
   ```

### Operational Commands

1. **Simulate** - Executes simulation analysis
   ```
   Simulate "SimulationName" {
       target: @"TargetElement"
       conditions: { /* simulation parameters */ }
       analyze: ["metric1", "metric2"]
   }
   ```

2. **Visualize** - Controls visualization output
   ```
   Visualize "VisualizationName" {
       target: @"TargetElement"
       mode: "visualizationType"
       property: "propertyToVisualize"
   }
   ```

3. **Query** - Extracts specific data
   ```
   Query "QueryName" {
       select: "elementType"
       from: @"SourceElement"
       where: "condition"
   }
   ```

4. **Import/Export** - Handles data exchange
   ```
   Import "ImportName" {
       source: "sourceType"
       format: "dataFormat"
       // Additional parameters
   }
   
   Export "ExportName" {
       target: @"TargetElement"
       format: "outputFormat"
       // Additional parameters
   }
   ```

## Use Case Scenarios

The BAML system is designed to support various architectural scenarios:

1. **Vertical Expansion Projects**
   - Adding new structures to existing rooftops
   - Creating elevated platforms spanning multiple buildings
   - Developing multi-level urban landscapes

2. **Adaptive Reuse**
   - Transforming industrial structures into mixed-use spaces
   - Converting obsolete infrastructure into community assets
   - Repurposing historical buildings with modern additions

3. **Urban Infill**
   - Integrating new structures within existing urban fabric
   - Creating connections between previously separate buildings
   - Developing transitional spaces between different urban typologies

4. **Evolutionary Architecture**
   - Designing buildings that adapt over time
   - Planning for phased development and transformation
   - Modeling the lifecycle of architectural interventions

## Implementation Strategy

The BAML system will be implemented as:

1. **Core Library** - A computational framework implementing the DSL parser, simulation engines, and data structures
2. **Plug-in Modules** - Extensions for popular design software (Rhino, Revit, etc.)
3. **Standalone Application** - A dedicated interface for users without specialized CAD software
4. **Web Service** - Cloud-based analysis and visualization capabilities

## Benefits and Impact

The Babylonian Architecture Mapping Library represents a paradigm shift in architectural planning by:

1. Enabling more sustainable approaches to urban development through adaptive reuse
2. Providing quantitative analysis to support innovative architectural interventions
3. Integrating social and physical dimensions of architecture in a unified framework
4. Supporting evolutionary approaches to building design and urban planning
5. Creating a common language for complex architectural propositions across disciplines

By treating architecture as a living, evolving system rather than static objects, BAML facilitates a new approach to urban development that respects existing structures while creating bold new architectural possibilities for the future.

# Babylonian Architecture Mapping DSL (BAMDSL)

## 1. Overview

BAMDSL is a domain-specific language designed for mapping and simulating architectural configurations in four dimensions, with special attention to integrating new Babylonian architectural patterns with existing structures. This DSL enables the definition, analysis, and simulation of both physical and social properties of architectural spaces over time.

## 2. Basic Syntax

BAMDSL uses a hierarchical structure with blocks and properties. The language supports:
- Nested blocks denoted by `{}` for hierarchical structures
- Key-value pairs for properties using `:`
- Arrays using `[]`
- Temporal sequences using `->` operator
- References to other elements with `@` prefix
- Comments with `//` for single line and `/* */` for multi-line

## 3. Core Components

### 3.1 Project Definition

```
Project "New Babylon Extension" {
    location: (34.5677, 40.1233)  // Coordinates
    baseDate: 2025-04-20          // Starting date for temporal modeling
    timeSpan: 50y                 // Project lifespan to simulate
    units: metric                 // Unit system
    baseMap: "ExistingCityModel"  // Reference to existing map
}
```

### 3.2 Material Definition

```
Material "Reinforced Concrete Type A" {
    density: 2400kg/m³
    compressiveStrength: 30MPa
    tensileStrength: 2.5MPa
    thermalExpansion: 0.000012/°C
    weatherResistance: 0.85
    lifespan: 75y
    deteriorationModel: @StandardConcreteDegradation
    fireResistance: 180min
    acousticAbsorption: 0.3
}
```

### 3.3 Structural Elements

```
Structure "Column-Type-A" {
    geometry: Cylinder {
        radius: 0.4m
        height: 3.5m
    }
    material: @"Reinforced Concrete Type A"
    loadCapacity: {
        axial: 500kN
        moment: 75kNm
        shear: 100kN
    }
    connections: [
        {type: "fixed", location: "bottom"},
        {type: "pin", location: "top"}
    ]
    failureMode: "BucklingAtMidHeight"
}
```

### 3.4 Space Definition

```
Space "Community Plaza" {
    geometry: Polygon {
        points: [(0,0,0), (20,0,0), (20,15,0), (0,15,0)]
        height: 5m
    }
    function: "gathering"
    capacity: 200people
    privacy: "public"
    boundaries: [
        {type: "open", side: "north"},
        {type: "partial", side: "east", height: 1.2m},
        {type: "structural", side: "south", element: @"ExistingWall-B"},
        {type: "structural", side: "west", element: @"Column-Row-A"}
    ]
    socialAttributes: {
        interaction: "high"
        circulation: "free"
        visibility: 0.9
        accessibility: "universal"
    }
    comfort: {
        thermal: "moderate"
        acoustic: "lively"
        lighting: "bright"
    }
}
```

### 3.5 Temporal Sequences

```
Evolution "PlazaTransformation" {
    phase(0y) {
        apply: @"Community Plaza"
        occupancy: 0.3
    }
    phase(2y) {
        extend: {
            direction: "north"
            distance: 10m
        }
        addElement: @"ShadingStructure-A"
        occupancy: 0.6
    }
    phase(5y) {
        transform: {
            function: "market"
            addElements: [@"StallStructure", @"CentralFountain"]
            capacity: 350people
        }
        socialAttributes: {
            interaction: "very-high"
            economic: "active"
        }
    }
}
```

### 3.6 Connections and Interfaces

```
Connection "NewTowerToExistingBuilding" {
    type: "structural-bridge"
    elements: [@"NewTower", @"ExistingBuilding-A"]
    points: [
        {element: @"NewTower", position: (5, 0, 15)},
        {element: @"ExistingBuilding-A", position: (0, 3, 12)}
    ]
    structure: @"ReinforcedBridge"
    loadTransfer: {
        vertical: 50kN
        horizontal: 25kN
        moment: 30kNm
    }
    circulation: {
        type: "pedestrian"
        capacity: 50people/h
        accessibility: "universal"
    }
}
```

### 3.7 Environmental Analysis

```
Environment "MicroclimateZone-A" {
    geometry: Box {
        origin: (10, 10, 0)
        dimensions: (50, 50, 30)
    }
    wind: {
        primaryDirection: (1, 0.5, 0)
        speed: 5m/s
        turbulence: 0.4
    }
    solar: {
        exposition: "high"
        shadingElements: [@"ExistingTower", @"NewCanopy"]
        annualPattern: @"SolarStudy-2025"
    }
    thermal: {
        baseTemperature: @"CityTemperatureModel"
        heatIslands: [
            {center: (20, 20, 0), intensity: 3°C, radius: 15m}
        ]
    }
    water: {
        drainagePattern: @"TerrainDrainageModel"
        collectionPoints: [@"RainwaterHarvesting-A"]
    }
}
```

### 3.8 Systems Integration

```
System "UtilityNetwork" {
    type: "electrical"
    capacity: 500kW
    nodes: [
        {type: "source", location: (0, 0, -1), capacity: 500kW},
        {type: "distribution", location: (50, 50, 0), capacity: 250kW},
        {type: "endpoint", location: (80, 30, 15), demand: 75kW}
    ]
    connections: [
        {start: "source", end: "distribution", capacity: 450kW},
        {start: "distribution", end: "endpoint", capacity: 100kW}
    ]
    redundancy: 0.3
    maintenanceAccess: [@"ServiceCorridor-A", @"ServiceShaft-B"]
}
```

## 4. Simulation Commands

```
Simulate "StructuralLoad" {
    target: @"NewBabylonianPlatform"
    conditions: {
        occupancy: "full"
        wind: {direction: (0, 1, 0), speed: 20m/s}
        seismic: {magnitude: 5.5}
    }
    analyze: ["deflection", "stress", "stability"]
    criticalThreshold: 0.85
    resolution: "high"
    outputFile: "structural-analysis-report.bam"
}

Simulate "SocialFlow" {
    target: @"CommercialDistrict"
    scenario: "Festival"
    parameters: {
        peakOccupancy: 500people/ha
        timespan: 5h
        entryPoints: [@"MainEntrance", @"SideAccess"]
    }
    analyze: ["congestion", "visibility", "interaction"]
    visualize: true
    temporalResolution: 15min
}

Simulate "TemporalEvolution" {
    target: @"EntireDevelopment"
    timespan: 20y
    intervals: 2y
    factors: ["material-aging", "usage-patterns", "weather-exposure"]
    analyze: ["structural-integrity", "space-adaptation", "maintenance-needs"]
}
```

## 5. Visualization Controls

```
Visualize "StructuralCapacity" {
    target: @"NewBabylonianPlatform"
    mode: "heatmap"
    property: "load-capacity-ratio"
    range: [0, 1]
    colorScale: ["green", "yellow", "red"]
    threshold: 0.8
    layers: ["structural-elements", "connections"]
}

Visualize "SocialActivity" {
    target: @"UrbanBlock-A"
    mode: "animation"
    property: "occupancy-density"
    timespan: 24h
    interval: 1h
    viewpoint: "isometric"
}
```

## 6. Query Language

```
Query "LoadCapacityCheck" {
    select: "structural-elements"
    from: @"ExistingBuilding-A"
    where: "load-capacity-ratio > 0.7"
    order: "load-capacity-ratio desc"
    limit: 10
}

Query "SocialHotspots" {
    select: "spaces"
    from: @"EntireDevelopment"
    where: "interaction-level > 0.8 AND privacy = 'public'"
    timeframe: "weekends"
    group: "function"
}
```

## 7. Data Integration

```
Import "ExistingCity" {
    source: "GIS-Database"
    format: "CityGML"
    location: "existing-city-model.gml"
    layers: ["buildings", "terrain", "infrastructure"]
    transform: {
        coordinates: "WGS84-to-local"
        units: "imperial-to-metric"
    }
}

Export "FinalModel" {
    target: @"EntireDevelopment"
    format: "BIM"
    detail: "LOD3"
    include: ["geometry", "properties", "systems"]
    temporalState: 5y
}
```

## 8. Example Complete Script

```
Project "New Babylon Community Hub" {
    location: (34.5677, 40.1233)
    baseDate: 2025-04-20
    timeSpan: 50y
    units: metric
    
    // Import existing city data
    Import "ExistingNeighborhood" {
        source: "CityDatabase"
        format: "GIS-3D"
        layers: ["buildings", "infrastructure", "terrain"]
    }
    
    // Define new materials
    Material "LightweightConcrete" {
        density: 1800kg/m³
        compressiveStrength: 25MPa
        tensileStrength: 2.0MPa
        thermalExpansion: 0.000010/°C
        weatherResistance: 0.8
        lifespan: 60y
    }
    
    Material "TensileMembranes" {
        density: 1.5kg/m²
        tensileStrength: 3000N/5cm
        fireResistance: "class-B"
        transparency: 0.3
        lifespan: 25y
    }
    
    // Define structural elements
    Structure "ElevatedPlatform" {
        geometry: Polygon {
            points: [(0,0,15), (30,0,15), (30,20,15), (0,20,15)]
            thickness: 0.5m
        }
        material: @"LightweightConcrete"
        loadCapacity: {
            distributed: 5kN/m²
            pointLoad: 20kN
        }
        supportPoints: [
            {location: (5,5,0), connection: @"ExistingBuilding-A"},
            {location: (25,5,0), connection: @"ExistingBuilding-B"},
            {location: (15,15,0), type: "new-column", material: @"ReinforcedConcrete"}
        ]
    }
    
    // Define spaces
    Space "CommunityGarden" {
        geometry: Polygon {
            points: [(5,5,15), (25,5,15), (25,15,15), (5,15,15)]
            height: 2m
        }
        function: "urban-agriculture"
        capacity: 50people
        soilDepth: 0.4m
        irrigation: @"RainwaterSystem"
        socialAttributes: {
            interaction: "medium"
            education: "high"
            nutrition: "local"
        }
    }
    
    Space "EventPlatform" {
        geometry: Polygon {
            points: [(5,5,15), (25,5,15), (25,15,15), (5,15,15)]
            height: 4m
        }
        function: "performance"
        capacity: 150people
        convertible: true
        socialAttributes: {
            interaction: "high"
            cultural: "medium"
            visibility: 0.8
        }
        
        // This space evolves over time
        Evolution "EventPlatformGrowth" {
            phase(0y) {
                apply: @"EventPlatform"
                occupancy: 0.5
            }
            phase(3y) {
                extend: {
                    direction: "east"
                    distance: 10m
                }
                addElement: @"CanopyStructure"
                capacity: 250people
            }
        }
    }
    
    // Define environmental analysis
    Environment "MicroclimateStudy" {
        geometry: Box {
            origin: (0, 0, 0)
            dimensions: (50, 50, 30)
        }
        wind: {
            primaryDirection: (1, 0.5, 0)
            speed: 5m/s
        }
        solar: {
            exposition: "high"
            shadingElements: [@"ExistingTower", @"CanopyStructure"]
        }
    }
    
    // Define simulation commands
    Simulate "StructuralAnalysis" {
        target: @"ElevatedPlatform"
        conditions: {
            occupancy: "full"
            wind: {direction: (0, 1, 0), speed: 15m/s}
        }
        analyze: ["deflection", "stress", "stability"]
    }
    
    Simulate "SocialInteraction" {
        target: @"CommunityGarden"
        scenario: "WeekendActivity"
        parameters: {
            peakOccupancy: 40people
            timespan: 8h
        }
        analyze: ["interaction", "circulation", "activity-patterns"]
    }
    
    // Define visualization
    Visualize "StructuralHealth" {
        target: @"EntireDevelopment"
        mode: "heatmap"
        property: "structural-integrity"
        range: [0, 1]
        colorScale: ["red", "yellow", "green"]
    }
    
    // Define query
    Query "CriticalConnections" {
        select: "connections"
        from: @"ElevatedPlatform"
        where: "load-ratio > 0.6"
        order: "load-ratio desc"
    }
}
```
