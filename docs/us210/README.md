# US210

### 1. User Story Description

I intend to list floors of a building

### 2. Customer Specifications and Clarifications

There was no need for customer clarifications

### 3. Diagrams

### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2

- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View](./level2/process-view.svg)

### Level 3

- [Logical View](../general-purpose/level3/logical-view.svg)
- [Process View](./level3/process-view.svg)
- [Implementation View](../general-purpose/level3/implementation-view.svg)
- [Class Diagram](./level3/class-diagram.svg)


### 4. HTTP

### 4.1 HTTP Requests

|   Method    |          HTTP request          |
|:-----------:|:------------------------------:|
| GET | /buildings/:id/floors |

### 4.2 HTTP Response

|  Status code  |       Description       |
|:-------------:|:-----------------------:|
|    **200**    |         List of floors from the desired building |
|    **404**    |       Building not found / Floor not found |

### 4.3 Authorization

n/a

### 4.4 HTTP Request Body

No request body is needed for this use case

### 4.5 HTTP Response Body

```json
[
    {
        "buildingCode": "B",
        "floorNumber": "1",
        "description": "Floor 1"
    },
    {
        "buildingCode": "B",
        "floorNumber": "2",
        "description": "Floor 2"
    },
    {
        "buildingCode": "B",
        "floorNumber": "4",
    }
]
```

## 5. Design Patterns

- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers

