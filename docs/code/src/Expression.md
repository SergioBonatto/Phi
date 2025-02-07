---
title: Expression
description: 'Definição do tipo de dados Expression para representar expressões lambda'
---

# Expression

Este módulo define o tipo de dados `Expression` que representa expressões lambda no cálculo lambda.

## Tipo de Dados

```haskell
data Expression
    = Var String
    | Lam String Expression
    | App Expression Expression
```

O tipo `Expression` tem três construtores:

- `Var String`: Representa uma variável.
- `Lam String Expression`: Representa uma abstração lambda (função anônima).
- `App Expression Expression`: Representa uma aplicação de função.

## Instância Show

Uma instância `Show` é definida para `Expression`, permitindo a conversão de expressões em strings legíveis:

- Variáveis são mostradas como seus nomes.
- Abstrações lambda são mostradas usando a notação "λx. e".
- Aplicações são mostradas entre parênteses.

## Exemplo de Uso

```haskell
let expr = App (Lam "x" (Var "x")) (Var "y")
print expr  -- Saída: ((λx. x) y)
```

Este módulo fornece a estrutura básica para representar e exibir expressões lambda, sendo fundamental para a implementação de um interpretador ou avaliador do cálculo lambda.