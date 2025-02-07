---
title: Setup
description: 'Configuração padrão do Cabal para o projeto Phi'
---

# Setup

Este arquivo contém a configuração padrão do Cabal para o projeto Phi.

## Conteúdo

```haskell
import Distribution.Simple
main = defaultMain
```

## Descrição

O arquivo `Setup.hs` é um componente padrão em projetos Haskell que utilizam o sistema de compilação Cabal. Ele define a função `main` que simplesmente chama `defaultMain` do módulo `Distribution.Simple`.

Esta configuração mínima instrui o Cabal a usar suas configurações padrão para compilar e gerenciar o projeto. Isso é suficiente para a maioria dos projetos Haskell e não requer personalização adicional.

O uso de `defaultMain` permite que o Cabal execute todas as tarefas padrão de compilação, teste e instalação sem necessidade de configuração adicional neste arquivo.