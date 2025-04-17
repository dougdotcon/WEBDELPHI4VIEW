# Guia de Instalação e Uso do PopularCaptchaTester

Este guia fornece instruções detalhadas sobre como compilar o projeto PopularCaptchaTester usando o Delphi 12 e como utilizar o sistema para testar e resolver captchas automaticamente.

## Índice

1. [Requisitos do Sistema](#requisitos-do-sistema)
2. [Instalação do WebView2 Runtime](#instalação-do-webview2-runtime)
3. [Configuração do Ambiente Delphi](#configuração-do-ambiente-delphi)
4. [Compilação do Projeto](#compilação-do-projeto)
5. [Solução de Problemas Comuns](#solução-de-problemas-comuns)
6. [Uso do Sistema](#uso-do-sistema)
7. [Configurações Avançadas](#configurações-avançadas)

## Requisitos do Sistema

Antes de começar, certifique-se de que seu sistema atende aos seguintes requisitos:

- Windows 10 ou 11 (64 bits recomendado)
- Delphi 12 (ou versão compatível)
- Microsoft Edge WebView2 Runtime
- Conexão com a Internet (para resolver captchas online)
- Chave de API para o serviço de resolução de captcha (opcional)

## Instalação do WebView2 Runtime

O WebView2 Runtime é essencial para o funcionamento do aplicativo, pois ele fornece o componente de navegador embutido.

1. Baixe o instalador do WebView2 Runtime no site oficial da Microsoft:
   [https://developer.microsoft.com/en-us/microsoft-edge/webview2/](https://developer.microsoft.com/en-us/microsoft-edge/webview2/)

2. Execute o instalador baixado e siga as instruções na tela.

3. Após a instalação, reinicie o computador para garantir que todas as configurações sejam aplicadas corretamente.

## Configuração do Ambiente Delphi

Para configurar o ambiente Delphi para compilar o projeto:

1. **Instalar a biblioteca WebView4Delphi**:
   
   A biblioteca WebView4Delphi já está incluída no projeto na pasta `BIN-WebView4Delphi`. No entanto, você precisa instalá-la no Delphi:

   a. Abra o Delphi 12
   b. Vá para `Component > Install Packages...`
   c. Clique em `Add...`
   d. Navegue até a pasta `BIN-WebView4Delphi\packages`
   e. Selecione o arquivo `WebView4DelphiVCLRTL.bpl` e clique em `Open`
   f. Repita o processo para o arquivo `WebView4DelphiVCL_designtime.bpl`
   g. Clique em `OK` para fechar a janela de instalação de pacotes

2. **Verificar a Library Path**:
   
   Certifique-se de que o Delphi pode encontrar os arquivos da biblioteca:

   a. Vá para `Tools > Options...`
   b. Navegue até `Language > Delphi > Library`
   c. Na seção `Library path`, adicione o caminho completo para a pasta `BIN-WebView4Delphi\source`
   d. Clique em `OK` para salvar as configurações

## Compilação do Projeto

Siga estas etapas para compilar o projeto:

1. **Abrir o Projeto**:
   
   a. No Delphi, vá para `File > Open...`
   b. Navegue até a pasta raiz do projeto
   c. Selecione o arquivo `PopularCaptchaTester.dproj` e clique em `Open`

2. **Verificar Configurações do Projeto**:
   
   a. Clique com o botão direito no projeto no Project Manager
   b. Selecione `Options...`
   c. Em `Building > Delphi Compiler > Output - Win32`, verifique se:
      - `Unit output directory` está configurado para `.\Win32\Debug\dcu`
      - `Output directory` está configurado para `.\Win32\Debug\bin`
   d. Clique em `OK` para salvar as configurações

3. **Compilar o Projeto**:
   
   a. Pressione `Shift+F9` ou vá para `Project > Compile PopularCaptchaTester`
   b. Aguarde a conclusão da compilação
   c. Verifique se não há erros na janela de mensagens

4. **Executar o Projeto**:
   
   a. Pressione `F9` ou vá para `Run > Run`
   b. O aplicativo deve ser iniciado sem erros

## Solução de Problemas Comuns

Se você encontrar problemas durante a compilação ou execução do projeto, tente estas soluções:

1. **Erro "WebView2Loader.dll não encontrado"**:
   
   Copie o arquivo `WebView2Loader.dll` da pasta `BIN-WebView4Delphi\bin32` para a pasta `Win32\Debug\bin`.

2. **Erro "Não é possível criar o formulário"**:
   
   Verifique se o arquivo `uMainForm.dfm` está presente e não está corrompido.

3. **Erro relacionado ao WebView2**:
   
   Certifique-se de que o Microsoft Edge WebView2 Runtime está instalado corretamente.

4. **Erros de compilação relacionados a unidades não encontradas**:
   
   Verifique se a Library Path do Delphi inclui o caminho para a pasta `BIN-WebView4Delphi\source`.

5. **Duplicação na criação do formulário**:
   
   Verifique se no arquivo `PopularCaptchaTester.dpr` não há duplicação na linha `Application.CreateForm(TMainForm, MainForm);`.

## Uso do Sistema

Após compilar e executar o projeto com sucesso, você pode usar o sistema da seguinte maneira:

1. **Interface Principal**:
   
   A interface do PopularCaptchaTester é dividida em várias seções:
   - Barra superior: URL e navegação
   - Painel central: Visualização do navegador
   - Painel direito: Log e configurações
   - Barra inferior: Status

2. **Carregar um Captcha**:
   
   a. Digite uma URL que contenha um captcha hCaptcha no campo de URL
   b. Clique no botão `Navigate`
   c. Alternativamente, clique em `Load Random Captcha` para carregar um dos exemplos pré-configurados

3. **Configurar o Solucionador**:
   
   a. No painel de configurações à direita, insira sua chave de API (se disponível)
   b. Configure as opções de acordo com suas preferências:
      - `Auto Solve`: Resolve automaticamente o captcha quando detectado
      - `Auto Open`: Abre automaticamente o captcha quando detectado
      - `English Language`: Força o idioma inglês para o captcha
      - `Always Solve`: Tenta resolver mesmo quando não tem certeza se é um captcha

4. **Resolver um Captcha**:
   
   a. Após carregar uma página com captcha, clique no botão `Solve Captcha`
   b. O sistema tentará detectar e resolver o captcha automaticamente
   c. O progresso e os resultados serão exibidos no painel de log

5. **Interromper o Processo**:
   
   Se necessário, clique no botão `Stop` para interromper o processo de resolução.

6. **Atualizar Iframes**:
   
   Se o captcha estiver em um iframe e não for detectado automaticamente, clique em `Refresh Iframes`.

7. **Salvar Configurações**:
   
   Clique em `Save Settings` para salvar suas configurações para uso futuro.

## Configurações Avançadas

O sistema oferece algumas configurações avançadas para usuários experientes:

1. **Método de Detecção**:
   
   Use o menu suspenso para selecionar o método de detecção de captcha:
   - `Automatique`: Detecta automaticamente o tipo de captcha
   - `Widget`: Força a detecção como widget simples
   - `MultiSelect`: Força a detecção como seleção múltipla
   - `Grid`: Força a detecção como grade de imagens
   - `Bbox`: Força a detecção como caixa delimitadora
   - `BboxDD`: Força a detecção como caixa delimitadora com arrastar e soltar

2. **Personalização da API**:
   
   Se você estiver usando um serviço de API personalizado, pode modificar o endpoint da API no código-fonte (`uPopularCaptchaSolver.pas`).

3. **Depuração**:
   
   O painel de log exibe informações detalhadas sobre o processo de resolução, o que pode ser útil para depuração.

---

Este guia deve ajudá-lo a compilar e usar o sistema PopularCaptchaTester com sucesso. Se você encontrar problemas adicionais ou tiver dúvidas, consulte a documentação na pasta `docs` ou entre em contato com o suporte.
