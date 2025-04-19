# Changelog - Testador de Captcha Popular

## Versão 1.1.7 - 28/07/2023

### Correções
- Corrigido erro de incompatibilidade de tipos "E2010 Incompatible types: 'TResultCallback' and 'Procedure'" modificando a chamada do método Solve
- Corrigido erro "E2010 Incompatible types: 'uPopularCaptchaSolver.TCaptchaDetectionMethod' and 'uMainForm.TCaptchaDetectionMethod'" removendo a declaração duplicada do tipo TCaptchaDetectionMethod
- Corrigidos erros "E2250 There is no overloaded version of 'Synchronize' that can be called with these arguments" ajustando os parâmetros do método Synchronize
- Alterada a visibilidade do método WndProc de private para protected para corresponder à visibilidade da classe base TForm
- Comentadas declarações de métodos não utilizados para eliminar avisos (hints) de compilação

### Modificações Técnicas
- Modificada a chamada de FCaptchaSolver.Solve para utilizar diretamente o método ProcessCaptchaResult como callback
- Adicionado prefixo de namespace (uPopularCaptchaSolver) ao tipo TCaptchaDetectionMethod para evitar ambiguidade
- Implementadas variáveis do tipo TThreadProcedure para uso com TThread.Synchronize
- Substituída a chamada TThread.Synchronize(nil, proc) por TThread.Synchronize(TThread.Current, proc) para corresponder à API atual

## Versão 1.1.6 - 24/07/2023

### Correções
- Criado um programa de diagnóstico simples para resolver problemas persistentes de compilação
- Removidas as dependências complexas para garantir que o projeto possa ser compilado

## Versão 1.1.5 - 24/07/2023

### Correções
- Corrigido erro "Missing implementation of interface method IWVLoaderEvents.doOnGetCustomSchemes" simplificando a implementação
- Corrigido erro "Could not compile used unit 'uWVCoreWebView2Delegates.pas'" removendo dependências complexas
- Simplificada a implementação para não depender do TWVLoader

## Versão 1.1.4 - 24/07/2023

### Correções
- Corrigido erro "File not found: 'uWVVersion.inc'" adicionando o arquivo no projeto

## Versão 1.1.3 - 24/07/2023

### Correções
- Corrigido erro "Could not compile used unit 'uWVLibFunctions'" adicionando arquivos auxiliares da biblioteca WebView4Delphi
- Implementada inicialização do GlobalWebView2Loader no arquivo WebView4Delphi.pas
- Resolvidos problemas com identificadores não declarados em uWVEvents.pas

## Versão 1.1.2 - 24/07/2023

### Correções
- Corrigido erro "Invalid compiler directive: 'UNITPATH'" e "File not found" copiando os arquivos necessários para a pasta raiz
- Simplificação da estrutura do projeto para compilação sem configurações adicionais

## Versão 1.1.1 - 24/07/2023

### Correções
- Corrigido erro de compilação "Could not compile used unit 'WebView4Delphi.source.uWVTypes'" ajustando as importações
- Eliminada referência de namespace com pontos que não é suportada pelo Delphi

## Versão 1.1.0 - 23/07/2023

### Correções
- Corrigido erro de compilação [dcc64 Fatal Error] F1026 "File not found: 'WebView4Delphi.pas'" criando um arquivo wrapper na raiz do projeto

### Melhorias
- Simplificado o sistema de importação da biblioteca WebView4Delphi
- Reorganizada a estrutura de dependências para melhor manutenção

### Modificações Técnicas
- Criado arquivo WebView4Delphi.pas na raiz do projeto que funciona como wrapper para os componentes da biblioteca
- Atualizada referência na cláusula 'uses' do arquivo PopularCaptchaTester.dpr
- Ajustadas as importações nos arquivos uMainForm.pas e uPopularCaptchaSolver.pas
- Removido arquivo uWVLoaderTypes.pas que não era mais necessário

### Informações Adicionais
- Mantida a compatibilidade com versões anteriores da aplicação
- Sem alterações de funcionalidades da interface do usuário 