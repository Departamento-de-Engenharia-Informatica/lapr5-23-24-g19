# US180

## 1. User Story Description

List buildings within a min and max floors

## 2. Customer Specifications and Clarifications

1. From [TEIXEIRA: [US180]Listar edifícios com min e max de pisos](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25150)

> > Bom dia, pode, por favor, explicar o que se pretende com esta US ?
>
> bom dia, permitir pesquisar que edificios tem no minimo X pisos e no máximo Y pisos

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)
-   [Process View](./level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)
-   [Process View](./level2/process-view.svg)

### Level 3

-   [Logical View](../general-purpose/level3/logical-view.svg)
-   [Process View](./level3/process-view.svg)
-   [Implementation View](../general-purpose/level3/implementation-view.svg)
-   [Class Diagram](./level3/class-diagram.svg)

## 4. HTTP

## 4.1 HTTP Requests

| Method |                HTTP request                 |
| :----: | :-----------------------------------------: |
|  GET   | /buildings/?minFloors={min}&maxFloors={max} |

## 4.2 HTTP Response

| Status code |                Description                |
| :---------: | :---------------------------------------: |
|   **200**   | list of buildings with min and max floors |

If there are no buildings, the system should return an empty list

### 4.3 HTTP Request Body

None; GET methods should not require a body

### 4.4 HTTP Response Body

```json
[
    {
        "code": "D",
        "name": "Building D",
        "description": "Example building",
        "maxFloorDimensions": { "length": 10, "width": 10 },
        "floorNumber": 5
    }
]
```

### 5. Design Patterns

-   Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

-   Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

-   Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

    -   The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

-   DTO: DTO's are used to transfer information between the different layers
