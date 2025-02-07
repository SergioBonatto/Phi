---
title: package.yaml
description: 'Arquivo de configuração do projeto Phi, definindo metadados, dependências e estrutura do pacote'
---

# package.yaml

Este arquivo contém a configuração do projeto Phi, utilizando o formato YAML para definir metadados, dependências e estrutura do pacote.

## Metadados do Projeto

- Nome: phi
- Versão: 0.1.0.0
- GitHub: githubuser/phi
- Licença: BSD-3-Clause
- Autor: "Author name here"
- Mantenedor: "example@example.com"
- Copyright: "2025 Author name here"

## Arquivos Adicionais

- README.md
- CHANGELOG.md

## Descrição

A descrição do projeto aponta para o README no GitHub.

## Dependências

- base >= 4.7 && < 5
- containers
- time
- HUnit

## Opções do GHC

Várias opções de compilação são definidas para garantir código de alta qualidade e evitar problemas comuns.

## Estrutura do Projeto

### Biblioteca

- Diretório de origem: src

### Executável

- Nome: phi
- Arquivo principal: Main.hs
- Diretório de origem: app

### Testes

- Nome: phi-test
- Arquivo principal: Spec.hs
- Diretório de origem: test

Tanto o executável quanto os testes dependem da biblioteca phi e incluem opções adicionais do GHC para threading e profiling.