# US 150 - Create building
### 1. User Story Description
Criar edifício

### 2. Customer Specifications and Clarifications
1. From [DIAS: US 150 Criar edifício](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25033#p31641)

> **Question:** Caro cliente,
Relativamente à criação de edifícios é suposto criar-mos um edifício sem nenhum piso inicialmente e depois adicionarmos os pisos?
Por exemplo: Criar o edifício A apenas, sem nenhum piso, e só depois na US 190 criar-mos os respetivos pisos do edifício A.
Ou é necessário sempre que criarmos um edifício especificar os pisos que o mesmo tem?
Por exemplo: Criar o edifício A, com os pisos A1, A2, A3 com as dimensões da grelha para cada um dos pisos.
Os melhores cumprimentos,
Grupo 002.

> **Answer:** boa tarde,
são dois requisitos independentes. 150 apenas define o edificio. posteriormente o utilizador invocará o caso de uso correspondete ao requisito 190 para criar cada piso desse edificio

2. From [JORGE: US 150 Criar edifício](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25047#p31835)

> **Question:**  Em relação à breve descrição, existe alguma regra em particular?

> **Answer:** bom dia é opcional, com o máximo de 255 caracteres

3. From [ALVES: US id310/190/230 Criar sala de piso de edifício](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25016#p31621)

> **Question:** Caro cliente,
Será possível esclarecer como funcionarão estas user stories? Com a 230 (Carregar mapa do piso) o nosso entendimento foi que as células seriam carregadas já com a criação de salas e pisos, e assim sendo não faria sentido as outras duas user stories, onde é pedido para criar um piso de um edifício e uma sala. Não entendemos o que é pretendido  com as us's 190 e 310.
Atentamente,
Grupo 63

> **Answer:** boa tarde,
o requisito 150 Criar edificio permite criar um edificio, exemplo, edificio "B", com um nome opcional e com uma breve descrição (ex., "departamento de engenharia informática") indicando a dimensão máxima de cada piso em termos de células (ex., 10 x 10)
o requisito 190 Criar piso permite definir um piso para um dos edificios criados anteriormente, por exemplo, o piso 1 do edificio B com uma breve descrição (ex., "salas TP")
o requisito 230 Carregar mapa de piso permite ao utlizador fazer upload de um ficheiro descrevendo o mapa de um dado piso. esse ficheiro deve ser validado se tem a estrutura correta e se obdece ao tamanho máximo definido aquando da criação do edificio
o requisito 310 Criar sala permite definir um sala num dado piso de um edificio, exemplo sala "B310" no 3º piso do edificio B, com uma categorização dessa sala (Gabinete, Anfiteatro, Laboratório, Outro) e uma breve descrição, ex., "Laboratório de Engenharia de Qualidade"

### 3. Diagrams
### Level 1

- [Logical View](../general-purpose/level1/logical-view.svg)
- [Process View](./level1/process-view.svg)

### Level 2
- [Logical View](../general-purpose/level2/logical-view.svg)
- [Process View](./level2/process-view.svg)

### Level 3
- [Logical View](../general-purpose/level3/logical-view.svg)
- [Process View](./level3/process-view.svg)
- [Implementation View](../general-purpose/level3/implementation-view.svg)

- [Class Diagram](./class-diagram.svg)

### 4. HTTP

### 4.1 HTTP Requests

| Method    | URI           |
|:---------:|:-------------:|
| **POST**  | /buildings    |

### 4.2 HTTP Response
| Status code   | Description |
|:-------------:|:-----------------------:|
| 201           | Building created |
| 422           | Building already exists / Invalid data (bad format/breaks business rules)   |

### 4.3 Authorization

N/a.

### 4.4 HTTP Request Body
```json
{
    "code": "B",
    "name": "Edifico B ",
    "description": "Informatica",
    "maxFloorDimensions": {
        "length": 18,
        "width": 10,
    }
}
```

### 4.5 Simulation HTTP requests

![postman](README/postman_buildingJ.JPG)
![mongo](README/mongo_buildingJ.JPG)
<!-- TODO: further improve? -->

### 5. Design Patterns

- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

### 6.Test

<!-- TODO -->

<!-- vim: set spelllang+=pt: -->
