# US 190

### 1. User Story Description

As an administrator, I intend to create a floor

### 2. Customer Specifications and Clarifications

**From the specifications document:**


**From the client clarifications:**

[ID 190 floor code](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25484)

[US id310/190/230 Criar sala de piso de edifício](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25016)

[US190 - Criar Piso de Edifício](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25248)

[US 190/240/270/310](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25188)

### 3. Diagrams

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

## 5. Design Patterns

[README.md](..%2Fgeneral-purpose%2FREADME.md)

## 6.Test
