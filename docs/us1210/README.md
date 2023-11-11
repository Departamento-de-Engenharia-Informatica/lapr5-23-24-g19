# US1080

## 1. User Story Description

This user story pertains to the functionality of listing buildings within a specified range of minimum and maximum floors.

## 2. Customer Specifications and Clarifications

The customer has outlined that, upon input of minimum and maximum floor numbers, the system is expected to generate a list of buildings that adhere to the specified criteria.

## 3. Diagrams

### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2
- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View](./level2/process-view.svg)

### Level 3
- [Logical View](../general-purpose/level3/logical-view.svg)
- [Process View](./level3/process-view.svg) [Work in Progress]
- [Implementation View](../general-purpose/level3/implementation-view.svg)
- [Class Diagram](./level3/class-diagram.svg) [Work in Progress]

## 4. Acceptance Criteria and Tests

To successfully fulfill this user story, the following criteria must be met:

- The user should be prevented from submitting the form if the minimum floor input exceeds the maximum floor input.
- The user should be prevented from submitting the form if anything other than an integer is entered.
- The user interface must effectively present the buildings along with their respective specifications and details.

## 5. Dependencies

This user case relies on [US180](../us180), which exposes the GET route of the backend API along with the required query string parameters:

```json
GET /buildings/?minFloors={min}&maxFloors={max}
```

## 5. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers
