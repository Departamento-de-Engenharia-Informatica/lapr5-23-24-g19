# US1030

## 1. User Story Description

> As Fleet Manager, I intend to query all the robots in the fleet

The list robots process was already
documented and implemented as per [us380](../us380/README.md).

Thus, this specific use case is solely concerned with the
development of a user interface (UI) that the fleet
manager can utilize.


## 2. Customer Specifications and Clarifications

Regarding what information should be presented when listing robots, see [us380](../us380/README.md).

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)
-   [Process View](./level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)
-   [Process View](./level2/process-view.svg)

### Level 3

-   [Logical View](../general-purpose/level3/logical-view.svg)
-   [Process View](./level3/process-view.svg) [Work in Progress]
-   [Implementation View](../general-purpose/level3/implementation-view.svg)
-   [Class Diagram](./level3/class-diagram.svg) [Work in Progress]

## 4. Acceptance Criteria and Tests

To successfully fulfill this user story, the following criteria must be met:

- The user interface must effectively present the robots along with their respective specifications and details.

## 5. Dependencies

This user case relies on [US380](../us380), which exposes the GET route of the backend API along with the required query string parameters:

```
GET /robots
```

## 6. Design Patterns

-   Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

-   Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

-   Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

    -   The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

-   DTO: DTO's are used to transfer information between the different layers
