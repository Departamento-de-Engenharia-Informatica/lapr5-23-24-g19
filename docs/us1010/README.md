# US1010

## 1. User Story Description

This user story addresses the needs of the Fleet Manager role, providing them with the capability to create robots, upon the selection of a robot type.

## 2. Customer Specifications and Clarifications

The customer has specified that the Fleet Manager should be able to create a robot, specifying its type, code, nickname, serialNumber, and an optional description.

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)
-   [Process View](./level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)
-   [Process View](./level2/process-view.svg)

### Level 3

-   [Logical View](../general-purpose/level3/logical-view.svg)
-   [Process View](./level3/process-view.svg) 
-   [Implementation View](../general-purpose/level3/implementation-view.svg)
-   [Class Diagram](./level3/class-diagram.svg) [Work in Progress]

## 4. Acceptance Criteria and Tests

To successfully complete this user story, the following criteria must be met:

-   The user interface should present available robot types and prompt the user to select the desired robot type to create a robot.
-   After selecting the robot type, the user interface should display the existing robots within that type.
-   The displayed robots should be updated every time the user changes the desired robot type.
-   The displayed robots should be updated every time the user creates a new one.
-   The user should be prevented from submitting the form if any of the values do not conform to the required types.

## 5. Dependencies

This user story relies on two API functionalities:

-   [list-robotTypes](../list-robotTypes)
    -   Used to GET existing robot types
    ```
    GET /robottypes
    ```
-   [US380](../us380)
    -   Used to GET existing robots
    ```
    GET /robots
    ```
-   [US360](../us350)
    -   Used to POST the newly created robot
    ```
    POST /robots
    ```

## 6. Design Patterns

-   Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

-   Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

-   Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

    -   The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

-   DTO: DTO's are used to transfer information between the different layers
