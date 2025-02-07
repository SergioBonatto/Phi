---
title: Environment
description: 'Módulo que define o tipo de ambiente para armazenamento de variáveis'
---

# Environment

Este módulo define o tipo `Env`, que representa o ambiente para armazenamento de variáveis no interpretador.

## Tipo

```haskell
type Env = Map String Expression
```

O tipo `Env` é definido como um `Map` que associa nomes de variáveis (representados como `String`) a expressões (`Expression`).

## Importações

- `Data.Map (Map)`: Importa o tipo `Map` da biblioteca `Data.Map` para implementar o mapeamento de variáveis.
- `Expression (Expression)`: Importa o tipo `Expression` do módulo `Expression` para representar os valores das variáveis.

## Uso

O ambiente (`Env`) é utilizado para armazenar e recuperar valores de variáveis durante a execução do interpretador. Ele permite associar nomes de variáveis a suas respectivas expressões, facilitando o gerenciamento do estado do programa.