# US460

## 1. User Story Description

This user story addresses the needs of the Client role, providing the capability to create both delivery and surveillance tasks.

## 2. Customer Specifications and Clarifications

The client has outlined that the Client Role should have the functionality to create two types of tasks: delivery tasks and surveillance tasks.
Surveillance tasks involves a specific floor from a building. Deliveries are done from room to room.
Contact information should be necessary for both tasks and additional Delivery Contact information should be given in Delivery type.
Furthermore, for deliveries, a Confirmation Code should be automatically generated.

## 3. Diagrams

### Level 1

-   [Logical View](../general-purpose/level1/logical-view.svg)

#### Surveillance

-   [Process View](./surveillance/level1/process-view.svg)

#### Delivery

-   [Process View](./delivery/level1/process-view.svg)

### Level 2

-   [Logical View](../general-purpose/level2/logical-view.svg)

#### Surveillance

-   [Process View](./surveillance/level2/process-view.svg)

#### Delivery

-   [Process View](./delivery/level2/process-view.svg)

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

##### Surveillance

-   [SPA Process View](./surveillance/level3/process-view-spa.svg)
-   [MDR Process View](./surveillance/level3/process-view-mdr.svg)
-   [MDT Process View](./surveillance/level3/process-view-mdt.svg)

##### Delivery

-   [SPA Process View](./delivery/level3/process-view-spa.svg)
-   [MDR Process View](./delivery/level3/process-view-mdr.svg)
-   [MDT Process View](./delivery/level3/process-view-mdt.svg)

## 4. Acceptance Criteria and Tests

To successfully complete this user story, the following criteria must be met:

### For Delivery

-   The user interface should be presented only when the user is logged in. Conversely, if the user is
    not logged in, the UI should communicate this information.
-   The user's email should be displayed after login.
-   The user interface should display fields such as building, floor, room and an optional description.
    Phone number and name are necessary for both pickup and delivery.
-   The submit button should be disabled if any of the parameters is empty.
-   The submit button should be disabled if any of the parameters is not respecting regex.

### For Surveillance

-   The user interface should be presented only when the user is logged in. Conversely, if the user is
    not logged in, the UI should communicate this information.
-   The user's email should be displayed after login.
-   The user interface should display fields such as building, floor, phone number and name.
-   The submit button should be disabled if any of the parameters is empty.
-   The submit button should be disabled if any of the parameters is not respecting regex.

## 5. Dependencies

This user story relies on three distinct API functionalities:

-   To retrieve buildings (MDR)

```
GET /buildings
```

-   To retrieve floors (MDR)

```
GET /buildings/{id}/floors
```

-   To retrieve rooms (MDR)

```
GET /buildings/{id}/floors/{floorNumber}/rooms
```

-   To post surveillance tasks (MDR)

```
POST /task/sequence
```

-   To post delivery tasks (MDR)

```
POST /task/delivery
```

-   To post tasks (MDT)

```
POST /jobs
```

## 6. Definition of Ready (DoR)

### 6.1 Clear and Detailed Description

The user story is deemed ready when the client's requisites are precisely articulated.
Specifically, the Client Role is expected to possess the capability to modify pertinent
details such as their name, phone number, and VAT number.

### 6.2 Acceptance Criteria

The criteria for acceptance are elucidated in section 4, encompassing conditions that must be
satisfied to consider the user story successfully implemented.

### 6.3 Dependencies and Resources

Dependencies are expounded upon in section 5, providing an overview of the requisite API functionalities
upon which the user story relies for its proper execution.

### 6.4 Estimation and Sizing

This user story is estimated to necessitate an allocation of approximately 8 to 10 hours for completion.
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

-   **Dependency Inversion:** Classes of one layer abstain from using specific implementations of a class from another layer, except for the domain. Instead, an interface defines a contract for communication.

-   **Dependency Injection:** Given the absence of explicit implementations, an injection mechanism dynamically determines which implementation to use based on a configuration file.

-   **Single Responsibility (Partially):** Each domain entity has a dedicated controller, service, and repository (interface) definition handling operations exclusively related to that entity. The partial application is due to the possibility of breaking down controllers/services by use case rather than by entity.

-   **DTO (Data Transfer Object):** DTOs are employed to transfer information seamlessly between different layers.
