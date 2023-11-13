# US1130

## 1. User Story Description

> As a Campus manager, I intend to create an elevator in a building, indicating the floors it serves.

The elevator creation and persistence processes were already
documented and implemented as per [us270](../us270/README.md).

Thus, this specific use case is solely concerned with the
development of a user interface (UI) that the campus
manager can utilize.

## 2. Customer Specifications and Clarifications

Regarding what information is required to create a elevator,
see [us270](../us270/README.md).

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

- The user interface must effectively present the building along with their respective specifications and details.
- The user interface must effectively present the floors inside the selected building along with their respective specifications and details.
- When the user provides valid data, the elevator should
  be successfully created (assuming no internal errors in the application backend)
    + Similarly, if invalid data is provided, the user should
      be informed of what went wrong



## 5. Dependencies


This user story depends upon the following use cases:

1. [US170](../us170), list buildings.
2. [US210](../us210), list floor(s) inside a specific building.
3. [US270](../us270),to create the elevator, which exposes the POST route of the backend API along with the required query string parameters:

```
POST /buildings/:id/elevators
```

## 6. Design Patterns

-   Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

-   Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

-   Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

    -   The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

-   DTO: DTO's are used to transfer information between the different layers
