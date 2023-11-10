# US200

## 1. User Story Description

Edit floor information

## 2. Customer Specifications and Clarifications

1. From [BRITO: US160, US200, US250 e US280](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25168)

> > Em relação às User Stories de edição, temos já uma ideia das informações que são opcionais, mas queremos ter a certeza daquilo que é editável ou não. Posto isto, poderia indicar que informações pretende editar nas US160, US200, US250 e US280?
> requisito 200 - editar piso - todas as informações à exceção do edificio a que o piso se refere

## 3. Diagrams

### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2

- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View](./level2/process-view.svg)

### Level 3

- [Logical View](../general-purpose/level3/logical-view.svg)
- [Process View for PATCH](./level3/process-view-patch.svg)
- [Process View for PUT](./level3/process-view-put.svg)
- [Implementation View](../general-purpose/level3/implementation-view.svg)
- [Class Diagram](./level3/class-diagram.svg)

## 4. HTTP

## 4.1 HTTP Requests

|    Method    | HTTP request |
|:------------:|:------------:|
| PATCH |    /buildings/{id}/floors/{floor}|
| PUT |     /buildings/{id}/floors/{floor}|


## 4.2 HTTP Response
| Status code |   Description   |
|:-----------:|:---------------:|
|   **404**   | building not found |
|   **404**   | floor not found |
|   **422**   | bad floor parameters |
|   **200**   | correct floor parameters |


### 4.3 HTTP Request Body

```json
{
    "floorNumber": 10,        #optional for patch
    "description": "example"  #optional for put and patch
}
```

### 4.4 HTTP Response Body

```json
{
    "buildingCode": "B",
    "floorNumber": "11",
    "description": "Changed to floor 11!"
}
```

### 5. Design Patterns

- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers
