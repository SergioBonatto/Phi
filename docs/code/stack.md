---
title: stack.yaml
description: 'Arquivo de configuração do Stack para o projeto Phi'
---

# stack.yaml

Este arquivo contém a configuração do Stack para o projeto Phi. O Stack é uma ferramenta de gerenciamento de projetos Haskell que simplifica o processo de construção, teste e gerenciamento de dependências.

## Configurações principais

- **Snapshot**: O projeto utiliza um snapshot do Stackage LTS 23.7, que define um conjunto consistente de pacotes e uma versão específica do compilador GHC.

```yaml
snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/23/7.yaml
```

- **Pacotes**: O projeto inclui apenas o pacote local (`.`), indicando que o código-fonte principal está no diretório atual.

```yaml
packages:
- .
```

## Outras configurações

O arquivo inclui comentários explicativos sobre várias opções de configuração adicionais que podem ser utilizadas, como:

- Adição de dependências extras (`extra-deps`)
- Substituição de flags padrão (`flags`)
- Uso de bancos de dados de pacotes adicionais (`extra-package-dbs`)
- Controle sobre a versão do GHC utilizada (`system-ghc`)
- Requisitos de versão do Stack (`require-stack-version`)
- Configurações de arquitetura (`arch`)
- Diretórios adicionais para inclusão e bibliotecas (`extra-include-dirs`, `extra-lib-dirs`)
- Verificação de versão do compilador (`compiler-check`)

Estas opções estão comentadas no arquivo, mas podem ser descomentadas e configuradas conforme necessário para ajustar o ambiente de construção do projeto.