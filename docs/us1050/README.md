# US 1050

## 1. User Story Description

> As a campus manager, I intend to create a building.

The building creation and persistence processes were already
documented and implemented as per [us150](../us150/README.md).

Thus, this specific use case is solely concerned with the
development of a user interface (UI) that the campus
manager can utilize.

## 2. Customer Specifications and Clarifications

Regarding what information is required to create a building,
see [us150](../us150/README.md)

## 3. Diagrams

### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2
- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View](./level2/process-view.svg)
- [Implementation](../general-purpose/level2/implementation-view.svg)

### Level 3
- [Logical View](../general-purpose/level3/ui-logical-view.svg)
- [Process View](./level3/process-view.svg)
- [Implementation View](../general-purpose/level3/ui-implementation-view.svg)

## 4. Acceptance Criteria

To successfully complete this user story, the following criteria must be met:

<!-- - The interface should be able to display/communicate to the -->
<!-- user what buildings already exist in the system. -->
<!--     + This list should be updated after a successful creation -->
- When the user provides valid data, the building should
be successfully created (assuming no internal errors in the application backend)
    + Similarly, if invalid data is provided, the user should
    be informed of what went wrong

## 5. Dependencies

This user story depends upon the following use cases:

1. US 150, building creation via the campus API.

## 6. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers
