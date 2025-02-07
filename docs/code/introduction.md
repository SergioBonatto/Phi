---
title: Introdução
description: 'Um guia para começar a usar o interpretador Phi de lambda cálculo'
---

# Phi - Um Interpretador de Lambda Cálculo

Phi é um interpretador leve para lambda cálculo puro, implementado em Haskell. Ele fornece um ambiente simples mas poderoso para experimentar com expressões de lambda cálculo e entender os fundamentos da computação funcional.

## Visão Geral

O Phi implementa os conceitos centrais do lambda cálculo:

- Avaliação de expressões lambda
- β-redução 
- Substituição de variáveis
- Avaliação preguiçosa (lazy evaluation)

O interpretador possui um parser robusto que lida com expressões aninhadas complexas, e oferece recursos como:

- Rastreamento detalhado da execução
- Inspeção do ambiente
- Limite configurável de passos de redução

## Instalação

Requisitos:
- [Stack](https://docs.haskellstack.org/en/stable/README/)

Para instalar:

```sh
git clone https://github.com/sergiobonatto/phi.git
cd phi
stack install
```

## Uso Básico

O Phi pode ser executado de duas formas:

1. Usando o Stack:
```
stack exec phi -- <arquivo> [-s] [-c]
```

2. Diretamente (se ~/.local/bin estiver no PATH):
```  
phi <arquivo> [-s] [-c]
```

Opções:
- `-s`: Exibe estatísticas de execução
- `-c`: Mostra o estado final do ambiente

Exemplo:

```sh
$ cat > exemplo.phi
let id = λx. x
(id λy. y)

$ phi exemplo.phi -s
=> λy. y
------------------
Passos: 2
Tempo: 0.000523s
```

Consulte a documentação completa para mais detalhes sobre a sintaxe, recursos avançados e arquitetura do interpretador.