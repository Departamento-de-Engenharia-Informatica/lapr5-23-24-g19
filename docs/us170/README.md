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

### 4.1.1 Body example

N/a

## 4.2 HTTP Response
| Status code |     Description     |
|:-----------:|:-------------------:|
|   **404**   | buildings not found |
|   **200**   | buildings returned  |

## 4.3 Authorization

No authorization required


### 4.4 HTTP Request Body


### 4.5 Simulation HTTP requests


### 5. Design Patterns

[README.md](..%2Fgeneral-purpose%2FREADME.md)

### 6.Test

- Test integration to ensure consistency between database and data returned
