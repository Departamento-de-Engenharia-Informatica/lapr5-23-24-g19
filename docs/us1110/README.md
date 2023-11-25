# US1110

## 1. User Story Description

This user story addresses the needs of the Campus Manager role, providing the capability to edit floors within a building, either partially or entirely.

## 2. Customer Specifications and Clarifications

The customer has specified that the Campus Manager should have the ability to edit both the floor number and an optional description upon selecting a building.

## 3. Diagrams

### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2
- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View (PATCH)](./level2/process-view-patch.svg)
- [Process View (PUT)](./level2/process-view-put.svg)

### Level 3
- [Logical View MDR](../general-purpose/level3/mdr-logical-view.svg)
- [Logical View UI](../general-purpose/level3/ui-logical-view.svg)
- [Implementation View MDR](../general-purpose/level3/mdr-implementation-view.svg)
- [Implementation View UI](../general-purpose/level3/ui-implementation-view.svg)
- [Process View (PATCH)](./level3/process-view-patch.svg)
- [Process View (PUT)](./level3/process-view-put.svg)
- [Class Diagram](./level3/class-diagram.svg) [Work in Progress]

## 4. Acceptance Criteria and Tests

To successfully complete this user story, the following criteria must be met:

- The user interface should present available buildings and prompt the user to select the desired building to edit a floor.
- Upon selecting the building, the user interface should display available floors and prompt the user to select the desired floor to edit.
- The displayed floors should be updated every time the user changes the desired building.
- The displayed floors should be updated every time the user edits a floor.
- The user should be prevented from submitting the form if a non-integer value is specified for the floor number.
- The user should be able to submit the form without specifying a description.
- The system should have the functionality to switch between editing the floor partially or entirely.

## 5. Dependencies

This user story relies on three different API functionalities:

- [US170](../us170)
    + Used to retrieve a list of every available building
    ```
    GET /buildings
    ```
- [US210](../us210)
    + Used to retrieve a list of every floor for a specific building
    ```
    GET /buildings/{id}/floors
    ```
- [US200](../us200)
    + Used to PATCH the edited floor (partial edit)
    ```
    PATCH /buildings/{id}/floors/{floor}
    ```
    + Used to PUT the edited floor (total edit)
    ```
    PUT /buildings/{id}/floors/{floor}
    ```

## 6. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers
