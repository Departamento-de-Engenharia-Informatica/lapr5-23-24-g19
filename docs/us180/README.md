# US180

## 1. User Story Description

List buildings within a min and max floors

## 2. Customer Specifications and Clarifications

1. From [TEIXEIRA: [US180]Listar edifícios com min e max de pisos](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25150)

> > Bom dia, pode, por favor, explicar o que se pretende com esta US ?
>
> bom dia, permitir pesquisar que edificios tem no minimo X pisos e no máximo Y pisos

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
| buildings    |   **GET**    |  /buildings/?minFloors={min}&maxFloors={max}|


## 4.2 HTTP Response
| Status code |   Description   |
|:-----------:|:---------------:|
|   **404**   | building(s) not found |
|   **200**   | building(s) found |

## 4.3 Authorization

Only Fleet Managers are allowed to execute this functionality


### 4.4 HTTP Request Body

n/a

### 4.5 HTTP Response Body

```json
{
    code: 'D',
    name: 'Building D',
    description: 'Example building',
    maxFloorDimensions: { length: 10, width: 10 },
    floorNumber: 5
}
```

### 5. Design Patterns

[README.md](..%2Fgeneral-purpose%2FREADME.md)


### 6.Test

