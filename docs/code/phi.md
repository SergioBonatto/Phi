---
title: phi.cabal
description: 'Arquivo de configuração Cabal para o projeto Phi'
---

# phi.cabal

Este arquivo contém a configuração Cabal para o projeto Phi. Ele define a estrutura, dependências e configurações de compilação do projeto.

## Informações Gerais

- Nome do projeto: phi
- Versão: 0.1.0.0
- Descrição: Veja o README no GitHub para mais informações
- Homepage: https://github.com/githubuser/phi#readme
- Rastreador de problemas: https://github.com/githubuser/phi/issues
- Autor: Author name here
- Mantenedor: example@example.com
- Copyright: 2025 Author name here
- Licença: BSD-3-Clause

## Estrutura do Projeto

O projeto é dividido em três componentes principais:

1. Biblioteca
2. Executável
3. Testes

### Biblioteca

A biblioteca expõe os seguintes módulos:

- Environment
- Evaluator
- Expression
- Main
- ParseApp
- ParseCommon
- ParseDefinition
- ParseError
- ParseExpr
- Parser
- ParseTypes
- ProcessCode
- Substitution
- Tokenize

### Executável

O executável principal está definido no arquivo `Main.hs` no diretório `app`.

### Testes

Os testes estão configurados para usar o framework HUnit e são definidos no arquivo `Spec.hs` no diretório `test`.

## Dependências

O projeto depende das seguintes bibliotecas:

- base (>=4.7 && <5)
- containers
- HUnit
- time

## Configurações de Compilação

O projeto usa várias flags de compilação para garantir código de alta qualidade:

- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Wpartial-fields
- -Wredundant-constraints

Além disso, o executável e os testes são configurados com as opções `-threaded`, `-rtsopts`, e `-with-rtsopts=-N` para suporte a multithreading.