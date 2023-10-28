# US 370

## 1. User Story Description

Como gestor de frota pretendo inibir um robot

## 2. Customer Specifications and Clarifications

N/a

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

| Method    | URI                   |
|:---------:|:---------------------:|
| **PATCH** | /robots/{:id}/inhibit |


## 4.2 HTTP Response
| Status code |   Description   |
|:-----------:|:---------------:|
|   **200**   | robot inhibited |
|   **404**   | robot not found |

## 4.3 Authorization
Only Fleet Managers are allowed to execute this functionality

### 4.4 HTTP Request Body
None; only the respective robot code is require to inhibit it, and that information is sent in the URI

### 4.5 HTTP Response Body
```json
{
    "code": "MVI24",
    "nickname": "george",
    "state": "Disabled",
    "serialNumber": "MDNM129AJ133",
    "description": "Lorem ipsum dolor sit amet",
    "typeCode": "Optimus"
}
```

### 5. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

### 6.Test
<!-- TODO -->
