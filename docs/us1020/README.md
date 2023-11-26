# US 1020

## 1. User Story Description

> As a fleet manager, I intend to inhibit a robot.

The robot inhibiting process was already
documented and implemented via an API as per [us370](../us370/README.md).

Thus, this specific use case is solely concerned with the
development of a user interface (UI) that the fleet
manager can utilize.

## 2. Customer Specifications and Clarifications

See [us370](../us370/README.md)

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

- The interface should be able to display/communicate to the user what robots
  exist in the system.
    + This content of the listing should be updated after a successful inhibition.
- After inhibiting, the selected robot should be in a disabled/inhibited state.
- The user can select an inhibited robot, to re-enable it.

## 5. Dependencies

This user story depends upon the following use cases:

1. US 360 and 1010, in order view the existing robots.
2. US 370, inhibit a robot via the fleet API.

> **NOTE:** As a simplification, only top-level dependencies were listed.
> The above use cases might also internally depend upon others.
## 6. Design Patterns
- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

<!-- TODO: review -->
- Single Responsibility (partially): Responsibilities are broken down by layers and by domain entity
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

- DTO: DTO's are used to transfer information between the different layers
