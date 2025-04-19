# Popular Captcha Tester

Aplicação para testar a resolução de captchas populares usando a biblioteca WebView4Delphi.

## Configuração do Projeto

Este projeto foi modificado para funcionar sem necessidade de configurações adicionais. Os arquivos principais da biblioteca WebView4Delphi foram copiados para a pasta raiz do projeto para simplificar a compilação.

## Estrutura do Projeto

- **PopularCaptchaTester.dpr**: Arquivo principal do projeto
- **uMainForm.pas**: Formulário principal da aplicação
- **uPopularCaptchaSolver.pas**: Classe responsável pela solução de captchas
- **WebView4Delphi.pas**: Arquivo wrapper para a biblioteca WebView4Delphi
- Arquivos auxiliares da biblioteca WebView4Delphi:
  - uWVBrowser.pas - Componente principal do WebView
  - uWVBrowserBase.pas - Classe base para o componente WebView
  - uWVConstants.pas - Constantes utilizadas pela biblioteca
  - uWVCoreWebView2.pas - Implementação do core da WebView2
  - uWVCoreWebView2Args.pas - Argumentos para os eventos do WebView2
  - uWVCoreWebView2Controller.pas - Controlador do WebView2
  - uWVCoreWebView2CustomSchemeRegistration.pas - Registros de esquemas personalizados
  - uWVCoreWebView2Delegates.pas - Delegados para eventos do WebView2
  - uWVCoreWebView2Environment.pas - Ambiente de execução do WebView2
  - uWVCoreWebView2EnvironmentOptions.pas - Opções de configuração do ambiente
  - uWVEvents.pas - Definição de eventos da biblioteca
  - uWVInterfaces.pas - Interfaces utilizadas pela biblioteca
  - uWVLibFunctions.pas - Funções auxiliares da biblioteca
  - uWVLoader.pas - Carregador da biblioteca WebView2
  - uWVLoaderInternal.pas - Funções internas do carregador
  - uWVMiscFunctions.pas - Funções diversas utilizadas pela biblioteca
  - uWVTypeLibrary.pas - Definição da biblioteca de tipos
  - uWVTypes.pas - Definição de tipos da biblioteca
  - uWVVersion.inc - Definições de versão da biblioteca
  - uWVWindowParent.pas - Componente pai da janela da WebView
  - webview2.inc - Definições para compilação condicional

## Compilação

Para compilar este projeto:
1. Abra o arquivo PopularCaptchaTester.dpr no Delphi IDE
2. Compile o projeto (Shift+F9)

## Troubleshooting

Se ainda ocorrerem erros de compilação, pode ser necessário copiar outros arquivos da pasta WebView4Delphi/source para a pasta raiz do projeto.

## Changelog

Veja o arquivo [CHANGELOG.md](CHANGELOG.md) para informações sobre as versões e alterações recentes. 