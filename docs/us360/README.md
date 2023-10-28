# US 360

## 1. User Story Description

Como gestor de frota pretendo adicionar um novo robot à frota indicando o seu tipo, designação, etc.

## 2. Customer Specifications and Clarifications

1. From [VEIGA: US 360 Adicionar um novo robot à frota](https://moodle.isep.ipp.pt/mod/forum/discuss.php?d=25265#p31991)
> > > > Os atributos do robot têm algum tipo de formatação/restrição?

> > > deve ser possivel corrigir todos os dados da passagem
> > > código identificativo, obrigatório, alfanumerico, max 30 caracteres, único no sistema
> > > nickname, obrigatório, obrigatório, alfanumerico, max 30 caracteres, único no sistema
> > > tipo de robot, obrigatório
> > > número de série, obrigatório, alfanumerico, max 50 caracteres, único para um dado tipo de robot
> > > descrição, opcional, alfanumerico, max. 250 caracteres
> >
> > Ao criar um novo robo, qual o estado dele por defeito, isto é, ativo ou inativo?
> ao criar um robot ele fica no estado ativo

**Interpretation:**
- The default state for a robot is "enabled"
- Several rules were laid out for each property of a robot

## 3. Diagrams
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

## 4. HTTP

## 4.1 HTTP Request

| Method    | URI       |
|:---------:|:---------:|
| POST      | /robots/  |

### 4.1.1 Body example
```json
{
    "code": "MVI24",
    "nickname": "george",
    "typeCode": "Optimus",
    "serialNumber": "MDNM129AJ133",
    "description": "Lorem ipsum dolor sit amet"
}
```
## 4.2 HTTP Response
| Status code   | Description                                       |
|:-------------:|:-------------------------------------------------:|
| 201           | Robot created                                     |
| 404           | Robot type not found                              |
| 422           | Invalid data (bad format/breaks business rules)   |

## 4.3 Authorization

N/a.

### 4.5 Simulation HTTP requests

<!-- TODO -->

### 5. Design Patterns

- Dependency inversion: Classes of one layer don't use specific implementations of a class from another layer (aside from domain); instead an interface defines a contract for how communications are made.

- Dependency injection: Since no explicit implementations are used, an injection mechanism takes care of deciding, at runtime, which implementation to use based on a configuration file.

- Single Responsibility (partially) - for each domain entity, there is a dedicated controller, service, repository (interface) definition that deals with/processes/handles operations related to that domain entity, and no other.
    + The reason it is a partial use lies in the fact that each controller/service could be broken down by use case rather than by entity

### 6.Test

<!-- TODO -->

<!-- vim: set spelllang+=pt: -->
