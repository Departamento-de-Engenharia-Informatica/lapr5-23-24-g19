# US 240 - Create Passage

### 1. User Story Description

As an administrator, I intend to create a passage between buildings.

## 2. Customer Specifications and Clarifications

**From the client clarifications:**

1. From [ALICE: US240 - Criar passagem](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25082#p31732)
**Question**:
>   Boa tarde caro cliente,
No mesmo edifício é possível ter duas passagens para outro edifício, em pisos diferentes?
Por exemplo, do edifício B existir uma passagem para o edifício G no piso 2 e outra no piso 3 também para o G.
Os melhores cumprimentos,
Grupo 63

**Answer**:
> bom dia,sim. em cada piso podem existir várias passagens, cada uma para outro edificio, e no mesmo edificio podem existir várias passagens para um outro edificio desde que em pisos diferentes"


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
| Method    | URI                           |
|:---------:|:-----------------------------:|
| **GET**   | /buildings/{:id}/elevators    |

### 4.1.1 Body example

N/a.

## 4.2 HTTP Response
| Status code   | Description           |
|:-------------:|:---------------------:|
| 404           | floor not found       |
| 422           | same building         |
| 422           | passage already exist |
| 200           | buildings returned    |

## 4.3 Authorization

N/a.

### 4.4 HTTP Request Body
```json
{
    "floor1":{
        "buildingCode": "A",
        "floorNumber": 2
    },
    "floor2":{
        "buildingCode": "B",
        "floorNumber": 2
    }
}
```
### 5. Design Patterns


Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file

Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

DTO: DTO's are used to transfer information between the different layers

Repository Pattern: The Repository pattern is used to abstract and encapsulate data access and database operations. It provides a consistent API for accessing data, which can be implemented differently in different layers.



### 6.Test

- Ensure that a passage can only be created between different buildings

