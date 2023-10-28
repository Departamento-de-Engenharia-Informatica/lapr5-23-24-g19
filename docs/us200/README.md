# US200

## 1. User Story Description

Edit floor information

## 2. Customer Specifications and Clarifications

1. From [BRITO: US160, US200, US250 e US280](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25168)

> > Em relação às User Stories de edição, temos já uma ideia das informações que são opcionais, mas queremos ter a certeza daquilo que é editável ou não. Posto isto, poderia indicar que informações pretende editar nas US160, US200, US250 e US280?
> requisito 200 - editar piso - todas as informações à exceção do edificio a que o piso se refere
## 3. Diagrams

### Level 1

Process View

![process-view.svg](level1%2Fprocess-view.svg)

### Level 2

Process View

![process-view.svg](level2%2Fprocess-view.svg)

### Level 3

Process View Patch
![process-view.svg](level3%2Fprocess-view-patch.svg)

Process View Put
![process-view.svg](level3%2Fprocess-view-put.svg)

Class Diagram

![class-diagram.svg](level3%2Fclass-diagram.svg)

## 4. HTTP

## 4.1 HTTP Requests

|    Method    | HTTP request |          URI          |
|:------------:|:------------:|:---------------------:|
| floors       |   **PATCH**  |  /buildings/{id}/floors/{floor}|
| floors       |   **PUT**    |  /buildings/{id}/floors/{floor}|


## 4.2 HTTP Response
| Status code |   Description   |
|:-----------:|:---------------:|
|   **404**   | building not found |
|   **404**   | floor not found |
|   **422**   | bad floor parameters |
|   **200**   | correct floor parameters |


### 4.3 HTTP Request Body

```json
{
    floorNumber: 10,        #optional for patch
    description: 'example'  #optional for put and patch
}
```

### 4.4 HTTP Response Body

```json
{
    buildingCode: 'B',
    floorNumber: '11',
    description: 'Changed to floor 11!'
}
```

### 5. Design Patterns

[README.md](..%2Fgeneral-purpose%2FREADME.md)


### 6.Test

