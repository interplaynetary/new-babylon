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
