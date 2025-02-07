---
title: Evaluator
description: 'Módulo responsável pela avaliação de expressões lambda'
---

# Evaluator

O módulo `Evaluator` é responsável pela avaliação de expressões lambda em um ambiente específico. Ele exporta a função principal `evaluate`.

## Função Principal

### evaluate

```haskell
evaluate :: Expression -> Env -> Set String -> Int -> (Expression, Int)
```

Esta função avalia uma expressão lambda em um ambiente dado, respeitando um número máximo de passos de redução.

Parâmetros:
- `Expression`: A expressão a ser avaliada
- `Env`: O ambiente de avaliação
- `Set String`: Conjunto de definições utilizadas (não usado nesta implementação)
- `Int`: Número máximo de passos de redução

Retorno:
- Uma tupla contendo a expressão avaliada e o número de passos realizados

## Funcionamento

A avaliação é realizada recursivamente, tratando diferentes casos:

1. Variáveis: Substituídas pelo seu valor no ambiente, se existir
2. Aplicações: Avalia o lado esquerdo e, se for uma lambda, aplica a substituição
3. Outros casos: Retorna a expressão sem alterações

A função também verifica se o número máximo de passos foi atingido, lançando um erro nesse caso.

## Dependências

O módulo utiliza:
- `Expression` para a definição de tipos de expressões
- `Environment` para o tipo de ambiente
- `Substitution` para realizar substituições
- `Data.Map` para manipulação do ambiente
- `Data.Set` para o conjunto de definições (não utilizado ativamente)