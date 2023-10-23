# US 240 - Create Passage

### 1. User Story Description

As an administrator, I intend to create a passage between buildings.

### 2. Customer Specifications and Clarifications

**From the specifications document:**

**From the client clarifications:**

>    Boa tarde caro cliente,
    No mesmo edifício é possível ter duas passagens para outro edifício, em pisos diferentes?
    Por exemplo, do edifício B existir uma passagem para o edifício G no piso 2 e outra no piso 3 também para o G.
    Os melhores cumprimentos,
    Grupo 63

> bom dia,sim. em cada piso podem existir várias passagens, cada uma para outro edificio, e no mesmo edificio podem existir várias passagens para um outro edificio desde que em pisos diferentes"

### 3. Diagrams


### 4. HTTP Requests

>POST /passages

```JSON
{
    "floor1":{
        "buildingCode": "A",
        "floorNumber": 2
    },
    "floor2":{
        "buildingCode": "B",
        "floorNumber": 2
    }
}
```
