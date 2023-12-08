
Projeto exemplo/template em ASP.NET Core 5.0 API para demonstrar como usar/aplicar (alguns conceitos de) DDD nesta tecnologia e usando também Entity Framework.
Adota um estilo arquitetural Onion.

Este exemplo compreende:
- um dominio com 3 aggregate roots (Category, Product e Family);
- um relacionamento de N <— 1 entre Category e Product;
- clara separação entre (i) API REST, (ii) Domínio e (iii) Infraestrutura (Persistência);
- aplicação de algumas regras de negócio/validação.

O dominio contempla as Entidade do negócio, os Serviços (ou casos de uso) envolvendo essas entidades e DTOs (in/out para os serviços de dominio).
Por simplicidade, empacotei fisicamente (packages) estas coisas por agregado.
Como é óbvio, outras alternativas de empacotamento são aceitáveis e (se calhar) desejáveis.

Também reconheço que podem ser introduzidas algumas melhorias interessantes com relativamente pouco esforço (mas de momento não tenho tempo).

