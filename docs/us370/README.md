# US-370

## 1. User Story Description

As an Fleet Manager, I intend to inhibit a robot.

## 2. Customer Specifications and Clarifications

N/a

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

|    Method    | HTTP request |          URI          |
|:------------:|:------------:|:---------------------:|
| inhibitRobot |   **PATCH**    |  /robots/:id/inhibit  |


## 4.2 HTTP Response
| Status code |   Description   |
|:-----------:|:---------------:|
|   **404**   | robot not found |
|   **200**   | robot inhibited |

## 4.3 Authorization

Only Fleet Managers are allowed to execute this functionality


### 4.4 HTTP Request Body

n/a

### 4.5 HTTP Response Body

```json
{
    "code": "MZC33",
    "nickname": "zuzu",
    "state": "Disabled",
    "serialNumber": "MDNM129AJ133",
    "description": "Citacao direta e plagio",
    "typeCode": "Optimus"
}
```

### 5. Design Patterns

[README.md](..%2Fgeneral-purpose%2FREADME.md)


### 6.Test

