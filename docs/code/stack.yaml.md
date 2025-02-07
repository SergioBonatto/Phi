---
title: stack.yaml.lock
description: 'Arquivo de lock gerado automaticamente pelo Stack para gerenciar dependências do projeto'
---

# stack.yaml.lock

Este arquivo é gerado e gerenciado automaticamente pelo Stack, uma ferramenta de build para projetos Haskell. Ele não deve ser editado manualmente.

## Conteúdo

O arquivo contém duas seções principais:

1. `packages`: Uma lista vazia neste caso, indicando que não há pacotes locais especificados.

2. `snapshots`: Especifica o snapshot Stackage usado pelo projeto.

### Snapshot

O snapshot utilizado é:

- URL: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/23/7.yaml
- SHA256: 4ef79c30b9efcf07335cb3de532983a7ac4c5a4180bc17f6212a86b09ce2ff75
- Tamanho: 680777 bytes

Este snapshot corresponde à versão LTS 23.7 do Stackage, que fornece um conjunto consistente de pacotes Haskell testados para trabalhar em conjunto.

## Propósito

O arquivo `stack.yaml.lock` garante que builds do projeto sejam reproduzíveis ao fixar versões exatas de dependências e do snapshot Stackage utilizado.