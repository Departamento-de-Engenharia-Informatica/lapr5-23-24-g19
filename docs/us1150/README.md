# US 1150

## 1. User Story Description

> As a campus manager, I intend to edit an elevator.

The elevator editing process was already
documented and implemented via an API as per [us280](../us280/README.md).

Thus, this specific use case is solely concerned with the
development of a user interface (UI) that the campus
manager can utilize.

## 2. Customer Specifications and Clarifications

Regarding what information is required to edit an elevator,
see [us280](../us280/README.md)

## 3. Diagrams

### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2
- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View](./level2/process-view.svg)
- [Implementation](../general-purpose/level2/implementation-view.svg)

### Level 3
- [Logical View](../general-purpose/level3/logical-view.svg)
- [Process View](./level3/process-view.svg) [Work in Progress]
- [Implementation View](../general-purpose/level3/implementation-view.svg)
<!-- - [Class Diagram](./level3/class-diagram.svg) [Work in Progress] -->

## 4. Acceptance Criteria

To successfully complete this user story, the following criteria must be met:

- The interface should be able to display/communicate to the
user what elevators exist within a building.
    + This content of the listing should be updated after a successful edit
- The interface should be able to provide a listing of the building's floors,
if the user intends on updating the floors covered by the elevator

- When the user provides valid data, the elevator should
be successfully updated (assuming no internal errors in the application backend)
    + Similarly, if invalid data is provided, the user should
    be informed of what went wrong
- The user should be able to partially edit the elevator, if the intent is to
only change a subset of it's characteristics

## 5. Dependencies

This user story depends upon the following use cases:

1. US 170 and 1060, in order to obtain a list of buildings
2. US 290 and 1140, in order to view the existing elevators within a building
3. US 210 and 1100, in order to obtain a list of floors within a building
    + The user might want to update which floors are covered by a given elevator
4. US 280, elevator editing via the campus API.

> **NOTE:** As a simplification, only top-level dependencies were listed.
> The above use cases might also internally depend upon others.
## 6. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

<!-- TODO: review -->
- Single Responsibility (partially): Responsibilities are broken down by layers and by domain entity
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers
