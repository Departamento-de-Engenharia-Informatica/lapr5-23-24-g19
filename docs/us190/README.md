# US 150 - Create building - Level 3

### 1. User Story Description

As an administrator, I intend to create a building.

### 2. Customer Specifications and Clarifications

**From the specifications document:**



**From the client clarifications:**


> **Question:**

> **Answer:**


### 3. Diagrams



### 4. HTTP

### 4.1 HTTP Requests

|   Method    |          HTTP request          |                         Description                         |
|:-----------:|:------------------------------:|:-----------------------------------------------------------:|
| createFloor | **POST** /buildings/:id/floors | Building Route calls method create floor in floorController |

### 4.2 HTTP Response
|  Status code  |       Description       |
|:-------------:|:-----------------------:|
|    **201**    |         Created         |
|    **422**    |  Unprocessable Content  |

### 4.3 Authorization

No authorization required

### 4.4 HTTP Request Body

[HTTP REQUEST BODY](./README/test.floors.txt)

### 4.5 Simulation HTTP requests

![postman](README/postman_buildingJ_floor1.JPG)
![mongo](README/mongo_buildingJ_floor1.JPG)
