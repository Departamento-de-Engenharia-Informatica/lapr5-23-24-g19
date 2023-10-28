# US 220

## 1. User Story Description
Listar pisos de edifício com passagem para outros edifícios

## 2. Customer Specifications and Clarifications
> > Relativamente a esta funcionalidade do sistema, seria expectável incluir informação relativa a onde a(s) passagem(ns) de cada piso vão ter; ou o pretendido é simplesmente ser possível saber quais dos pisos de um dado edifício possuem passagens para outros?
> esta listagem deve mostrar a informação sobre o piso (edificio, piso, descrição) e a que outros edificios/pisos tem passagem

**Interpretation:** The respective passage(s) should be listed together with the floor that has them

## 3. Diagrams

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

- [Class Diagram](./class-diagram.svg)

## 4. HTTP

## 4.1 HTTP Requests
| Method    | URI                               |
|:---------:|:---------------------------------:|
| GET       | /buildings/{:id}/floors/passages  |

## 4.2 HTTP Response
| Status code |   Description   |
|:-----------:|:---------------:|
|   **200**   | list of floors from the given building with passages |
|   **404**   | building not found/no results found |

## 4.3 Authorization
N/a.

### 4.4 HTTP Request Body
None; GET methods should not require a body

### 4.5 HTTP Response Body
```json
[
    {
        "floor": {
            "buildingCode": "A",
            "floorNumber": 1,
            "description": "admin"
        },
        "passages": [
            {
                "buildingCode": "B",
                "floorNumber": 2,
                "description": "labs"
            },
            {
                "buildingCode": "B",
                "floorNumber": 1,
                "description": "admin"
            }
        ]
    },
    {
        "floor": {
            "buildingCode": "A",
            "floorNumber": 2,
            "description": "labs"
        },
        "passages": [
            {
                "buildingCode": "B",
                "floorNumber": 2,
                "description": "admin"
            }
        ]
    }
]
```

### 5. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

### 6.Test
<!-- TODO -->
