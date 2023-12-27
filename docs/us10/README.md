# US10

## 1. User Story Description

As an administrator, I want to create a system user and specify their permissions.

## 2. Customer Specifications and Clarifications

The client has outlined that the Administrator Role should possess the functionality to create backoffice(or system) users.

The administrator assigns the role during the creation of backoffice users:

- Fleet manager
- Campus manager
- Task manager
- Administrator

Attributes are :

- Name
- Email(with @isep.ipp.pt)
- Phone number (with 9 digits)
- Password

Password created by the administrator with minimum of 10 characters:

- At least 1 uppercase letter
- At least 1 lowercase letter
- At least 1 digit
- At least 1 symbol

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)
-   [Process View](./level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)
-   [Process View ](./level2/process-view.svg)

### Level 3

#### Logical Views

-   [MDR Logical View](../general-purpose/level3/mdr-logical-view.svg)
-   [UI Logical View](../general-purpose/level3/ui-logical-view.svg)

#### Implementation Views

-   [MDR Implementation View](../general-purpose/level3/mdr-implementation-view.svg)
-   [UI Implementation View](../general-purpose/level3/ui-implementation-view.svg)

#### Process Views

-   [MDR Process View](./level3/be-process-view.svg)
-   [SPA Process View](./level3/fe-process-view.svg)
-   [Class Diagram View](./level3/be-class-diagram.svg)

## 4. Acceptance Criteria and Tests

To successfully complete this user story, the following criteria must be met:

-   The admin interface should be presented only when the admin is logged in. Conversely, if the admin is not logged in, the UI should communicate this information.
-   The creation of a backoffice user should require the specifications mentioned above.
-   The submit button should be disabled if one of the parameters is not accordingly with the specifications mentioned above.
-   Placeholders containing the actual data should be populated in each textbox.

## 5. Dependencies

This user story relies on three distinct API functionalities:

-   To create backoffice user
    ```
    POST /users-backoffice
    ```

## 6. Design Patterns

-   **Dependency Inversion:** Classes of one layer abstain from using specific implementations of a class from another layer, except for the domain. Instead, an interface defines a contract for communication.

-   **Dependency Injection:** Given the absence of explicit implementations, an injection mechanism dynamically determines which implementation to use based on a configuration file.

-   **Single Responsibility (Partially):** Each domain entity has a dedicated controller, service, and repository (interface) definition handling operations exclusively related to that entity. The partial application is due to the possibility of breaking down controllers/services by use case rather than by entity.

-   **DTO (Data Transfer Object):** DTOs are employed to transfer information seamlessly between different layers.


