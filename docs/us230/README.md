# US 230 - Upload Map

## 1. User Story Description

As an administrator, I intend to upload a floor's map.

## 2. Customer Specifications and Clarifications

**From the specifications document:**

**From the client clarifications:**

[Fórum: US230 - Carregar mapa de piso](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25070#p31747)

[Map example](https://www.dei.isep.ipp.pt/~jpp/LAPR5/mazes/LAPR5.json)

[Descrição do projeto](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25046)

> "de momento podem optar por assumir que as passagens e os elevadores ocupam sempre duas células e que apenas é necessário indicar a célula de "topo" dessas duas"

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

|  Method   | HTTP request |                 URI                 |
|:---------:|:------------:|:-----------------------------------:|
| updateMap |  **PATCH**   | /buildings/{id}/floors/{number}/map |


## 4.2 HTTP Response
| Status code |    Description    |
|:-----------:|:-----------------:|
|   **404**   |  floor not found  |
|   **422**   | map not processed |
|   **200**   |    map updated    |

## 4.3 Authorization

No authorization required


### 4.4 HTTP Request Body

[HTTP REQUEST BODY](./a.JSON)


### 4.5 Simulation HTTP requests


### 5. Design Patterns

[README.md](..%2Fgeneral-purpose%2FREADME.md)

### 6.Test

-


