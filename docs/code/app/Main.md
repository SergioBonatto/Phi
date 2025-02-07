---
title: Main
description: 'Módulo principal do interpretador Phi, responsável por processar argumentos de linha de comando e executar o código Phi'
---

# Main

O módulo `Main` é o ponto de entrada do interpretador Phi. Ele lida com os argumentos da linha de comando, processa o código fonte Phi e exibe os resultados da execução.

## Funcionalidades principais

1. Processamento de argumentos da linha de comando
2. Leitura do arquivo de código fonte
3. Análise e avaliação do código Phi
4. Exibição dos resultados e estatísticas opcionais

## Uso

```
phi <file> [-s] [-c]
```

- `<file>`: Caminho para o arquivo de código fonte Phi
- `-s`: (Opcional) Exibe estatísticas de execução
- `-c`: (Opcional) Exibe o contexto de definições utilizadas

## Fluxo de execução

1. Verifica os argumentos da linha de comando
2. Lê o conteúdo do arquivo de código fonte
3. Processa o código usando `processCode`
4. Avalia a expressão resultante usando `evaluate`
5. Exibe o resultado final
6. Se solicitado, exibe estatísticas e contexto

## Detalhes de implementação

- Utiliza `getCurrentTime` para medir o tempo de execução
- Limita a avaliação a 1000 passos de redução
- Gerencia erros de análise e exibe mensagens apropriadas

Este módulo integra as funcionalidades de análise e avaliação do Phi, fornecendo uma interface de linha de comando para o interpretador.