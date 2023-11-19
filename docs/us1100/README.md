# US1100

## 1. User Story Description

This user story focuses on the functionality of listing floors associated with a specified building.

## 2. Customer Specifications and Clarifications

The customer has specified that the system should generate a comprehensive list of floors belonging to a building, as designated by the Campus Manager.

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)
-   [Process View](./level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)
-   [Process View](./level2/process-view.svg)

### Level 3

-   [Logical View MDR](../general-purpose/level3/mdr-logical-view.svg)
-   [Logical View UI](../general-purpose/level3/ui-logical-view.svg)
-   [Implementation View MDR](../general-purpose/level3/mdr-implementation-view.svg)
-   [Implementation View UI](../general-purpose/level3/ui-implementation-view.svg)
-   [Process View](./level3/process-view.svg)
-   [Class Diagram](./level3/class-diagram.svg) [Work in Progress]

## 4. Acceptance Criteria and Tests

To successfully complete this user story, the following criteria must be met:

-   The user interface must adeptly present the floors, along with their respective specifications and details.

## 5. Dependencies

This user story relies on [US170](../us180), which provides the GET route for buildings in the backend API:

```
GET /buildings
```

Additionally, this user story depends on [US210](../us210), responsible for exposing the GET route for floors from a specific building:

```
GET /buildings/{id}/floors
```

## 6. Design Patterns

-   Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

-   Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

-   Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

    -   The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

-   DTO: DTO's are used to transfer information between the different layers
