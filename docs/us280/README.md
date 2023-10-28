# US 280 - Edit elevator in a building.

### 1. User Story Description

I intend to edit an elevator in a building.

### 2. Customer Specifications and Clarifications

**From the specifications document:**

**From the client clarifications:**

> **Question:** Caro cliente,
Dentro de uma discussão do fórum, encontrei esta informação quanto ao requisito 280:
"requisito 280 - editar elevador - todas as informações à exceção do edificio a que o piso se refere"
Como tal, gostaria de saber que atributos deveria ter o elevador, para além de uma lista de pisos aos quais consegue aceder dentro do seu edifício. Algumas das ideias que me surgiram foram o piso em que estava localizado naquele momento, número de série, fabricante ou descrição.
Obrigado desde já,
Grupo 002.

> **Answer:**
bom dia,
edificio (obrigatório)
número identificativo (obrigatório, único no edificio)
lista de pisos do edificio servidos pelo elevador (obrigatório)
marca (opcional, alfanumerico, 50 caracteres)
modelo (opcional, mas obrigatório se marca for introduzido, alfanumerico, 50 caracteres)
número de série do fabricante (opcional, alfanumerico, 50 caracteres)
breve descrição (opcional, alfanumerico, 250 caracteres)


### 3. Diagrams

![level1](level1/process-view.svg)
![level2](level2/process-view.svg)
![level3](level3/process-view-put.svg)
![level3](level3/process-view-patch.svg)
![level3](level3/class-diagram.svg)

### 4. HTTP

### 4.1 HTTP Requests

|    Method     |                HTTP request                |                          Description                           |
|:-------------:|:------------------------------------------:|:--------------------------------------------------------------:|
|  putElevator  |  **PUT** /buildings/{idb}/elevators/{ide}  | Building Route calls method getElevators in elevatorController |
| patchElevator | **PATCH** /buildings/{idb}/elevators/{ide} | Building Route calls method getElevators in elevatorController |

### 4.2 HTTP Response
| Status code |      Description      |
|:-----------:|:---------------------:|
|   **200**   |          OK           |
|   **404**   |       Not Found       |
|   **422**   | Unprocessable Content |

### 4.3 Authorization

No authorization required

### 4.4 HTTP Request Body

[HTTP REQUEST BODY](./README/test.elevators.txt)

```typescript

route.put(
    '/:idb/elevators/:ide',
    celebrate({
        body: Joi.object({
            floors: Joi.array()
                .items(
                    Joi.number()
                        .integer()
                        .required(),
                )
                .required(),
            brand: Joi.string(),
            model: Joi.string(),
            serialNumber: Joi.string(),
            description: Joi.string(),
        }),
    }),
    (req, res, next) => elevatorCtrl.putElevator(req, res, next),
)

```

```typescript

route.patch(
    '/:idb/elevators/:ide',
        celebrate({
            body: Joi.object({
                floors: Joi.array()
                        .items(
                Joi.number()
                        .integer()
                        .required(),
                )
                .optional(),
                brand: Joi.string(),
                model: Joi.string(),
                serialNumber: Joi.string(),
                description: Joi.string(),
            }),
        }),
    (req, res, next) => elevatorCtrl.patchElevator(req, res, next),
)
```

### 4.5 Simulation HTTP requests

## 4.5.1 PATCH

![mongo](README/mongo_inicial.JPG)

![postman](README/postman_PATCH.JPG)

![mongo](README/mongo_PATCH.JPG)

## 4.5.2 PUT

![mongo](README/mongo_inicial.JPG)

![postman](README/postman_PUT.JPG)

![mongo](README/mongo_PUT.JPG)


### 5. Design Patterns

- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity



