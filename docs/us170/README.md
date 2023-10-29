# US-170

## 1. User Story Description

As an administrator, i want to list all buildings.

## 2. Customer Specifications and Clarifications

**From the client clarifications:**

## 3. Diagrams

### Level 1

Process View

![process-view.svg](level1%2Fprocess-view.svg)

### Level 2

Process View

![process-view.svg](level2%2Fprocess-view.svg)

### Level 3

Process View

![process-view.svg](level3%2Fprocess-view.svg)

Class Diagram

![class-diagram.svg](level3%2Fclass-diagram.svg)

## 4. HTTP

## 4.1 HTTP Requests

|    Method    | HTTP request |                            URI                             |
|:------------:|:------------:|:----------------------------------------------------------:|
| getBuildings |   **GET**    |                         /buildings                         |

## 4.2 HTTP Response
| Status code |     Description     |
|:-----------:|:-------------------:|
|   **404**   | buildings not found |
|   **200**   | buildings returned  |

## 4.3 Authorization

No authorization required


### 4.4 HTTP Request Body

N/A


### 5. Design Patterns


Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file

Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.

DTO: DTO's are used to transfer information between the different layers

Repository Pattern: The Repository pattern is used to abstract and encapsulate data access and database operations. It provides a consistent API for accessing data, which can be implemented differently in different layers.



### 6.Test

- Test integration to ensure consistency between database and data returned
