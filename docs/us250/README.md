# US 250

## 1. User Story Description

Editar passagem entre edifícios

## 2. Customer Specifications and Clarifications

1. From [TEIXEIRA: US250 - Editar passagem entre edifícios](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25164#p31860)
> > > > relativamente à US250 o que é que pretende que seja possivel editar numa passagem entre edificios?
> > > deve ser possivel corrigir todos os dados da passagem
> > Seria possível dizer o que pretende indicar no que se refere à passagem entre edifícios? Seria apenas os edifícios e os pisos referentes à mesma, ou deve ser dito mais alguma coisa acerca de uma passagem?
> apenas os edificios e os pisos que estão ligados por essa passagem

**Interpretation:** Only the buildings/floors connected by the passage can be changed.

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

## 4.1 HTTP Request

| Method    | URI       |
|:---------:|:---------:|
| PATCH     | /passages |
| PUT       | /passages |

### 4.1.1 Body example
```json
{
    "floor1": {
        "buildingCode": "A",
        "floorNumber": 1
    },
    "floor2": {
        "buildingCode": "B",
        "floorNumber": 2
    }
}
```

## 4.2 HTTP Response
| Status code   | Description                                       |
|:-------------:|:-------------------------------------------------:|
| 200           | Passage edited                                    |
| 404           | Buildings/floors/passage not found                |
| 422           | Invalid data (bad format/breaks business rules)   |

## 4.3 Authorization

N/a.

### 4.5 Simulation HTTP requests

<!-- TODO -->

### 5. Design Patterns

- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

### 6.Test

<!-- TODO -->

<!-- vim: set spelllang+=pt: -->
