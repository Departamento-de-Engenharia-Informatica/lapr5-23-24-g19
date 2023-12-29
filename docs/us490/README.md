# US490

## 1. User Story Description

This user story addresses the needs of the Task Manager role, providing the capability to list tasks based on filters

## 2. Customer Specifications and Clarifications


The client has outlined that the Task Manager Role should have the functionality to list tasks based on filters that match
a given filter, which can be made following 3 criterion specified by the client:
state where it can be Pending, Accepted,Rejected or Planned
client where the filter is made by the user e-mail
type where the task type is the filter

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)

#### Surveillance

-   [Process View](./surveillance/level1/process-view.svg)

#### Delivery

-   [Process View](./delivery/level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)
-   [Process View](./level2/process-view.svg)

### Level 3

#### Logical Views

-   [SPA Logical View](../general-purpose/level3/ui-logical-view.svg)
-   [MDR Logical View](../general-purpose/level3/mdr-logical-view.svg)
-   [MDT Logical View](../general-purpose/level3/mdt-logical-view.svg)

#### Implementation Views

-   [SPA Implementation View](../general-purpose/level3/ui-implementation-view.svg)
-   [MDR Implementation View](../general-purpose/level3/mdr-implementation-view.svg)
-   [MDT Implementation View](../general-purpose/level3/mdt-implementation-view.svg)

#### Process Views

-   [SPA Process View](./level3/process-view-spa.svg)
-   [MDR Process View](./level3/process-view-mdr.svg)
-   [MDT Process View](./level3/process-view-mdt.svg)

## 4. Acceptance Criteria and Tests

To successfully complete this user story, the following criteria must be met:

-   The user interface should present a list of the different criterion that can be used and asks the user to choose
-   When the user criteria is chosen, allow the user to write the email to be used on the filter
-   When the state criteria is chosen, allow the user to choose which of the states are to be used
-   When the type criteria is chosen, allow the user to choose which of the types are to be used
-   When no tasks meet the filter a message should be displayed

## 5. Dependencies

This user story relies on three distinct API functionalities:

-   To retrieve tasks by filter (MDT)

```
POST /jobs/filter?criteria={criteria}&filter={filter}
```
## 6. Definition of Ready (DoR)

### 6.1 Clear and Detailed Description

The user story is deemed ready when the client's requisites are precisely articulated.
Specifically that each of the criteria have a specific way to handle information.

### 6.2 Acceptance Criteria

The criteria for acceptance are elucidated in section 4, encompassing conditions that must be
satisfied to consider the user story successfully implemented.

### 6.3 Dependencies and Resources

Dependencies are expounded upon in section 5, providing an overview of the requisite API functionalities
upon which the user story relies for its proper execution.

### 6.4 Estimation and Sizing

This user story is estimated to necessitate an allocation of approximately 4 to 6 hours for completion.
This estimate is derived from a comprehensive assessment of the complexity and scope inherent in the
tasks involved.

## 7. Definition of Done (DoD)

### 7.1 Code Quality

The user story is considered done when the developed code aligns with established coding standards
and adheres to recognized engineering best practices. This encompasses the use of consistent and
maintainable code structures.

### 7.2 Testing

Completion of the user story necessitates thorough testing, including Component and End-to-End (E2E)
testing for the Single Page Application (SPA) module. Additionally, Unit and Integration testing must be
conducted for the MDR module to ensure robust functionality.

### 7.3 Documentation

The user story is considered concluded when comprehensive documentation is provided. This
documentation encompasses Process Views detailing the workflow of the use case, coupled with
Implementation and Logical Views offering insights into the technical underpinnings of the solution.
This documentation ensures transparency and facilitates future understanding and maintenance of
the implemented features.

## 8. Design Patterns


-   **Strategy Pattern** allows the dynamic selection of a search algorithm at runtime. This is when different types of filters need to be applied based on user preferences.
    The dynamic nature of the strategy pattern enables the system to adapt to varying search requirements without the need for conditional statements or complex switch-case structures.
-   **Dependency Inversion:** Classes of one layer abstain from using specific implementations of a class from another layer, except for the domain. Instead, an interface defines a contract for communication.

-   **Dependency Injection:** Given the absence of explicit implementations, an injection mechanism dynamically determines which implementation to use based on a configuration file.

-   **Single Responsibility (Partially):** Each domain entity has a dedicated controller, service, and repository (interface) definition handling operations exclusively related to that entity. The partial application is due to the possibility of breaking down controllers/services by use case rather than by entity.

-   **DTO (Data Transfer Object):** DTOs are employed to transfer information seamlessly between different layers.
