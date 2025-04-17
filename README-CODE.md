# CaptchaSolver (CODE-hcaptcha-delphiweb)

## Visão Geral
O projeto **CaptchaSolver** é uma aplicação Delphi especializada na resolução automatizada de captchas hCaptcha, utilizando um navegador sem janela (windowless browser) baseado na tecnologia WebView2 da Microsoft. Este projeto implementa uma abordagem mais estruturada e modular para resolver captchas em comparação com o projeto PopularCaptchaTester.

## Tecnologias Utilizadas
- **Delphi**: Compatível com versões recentes do Delphi
- **WebView4Delphi**: Biblioteca para incorporação do navegador
- **Microsoft Edge WebView2 Runtime**: Mecanismo de renderização web
- **DirectComposition API**: Para implementação do navegador sem janela
- **JSON**: Para comunicação com APIs e processamento de dados

## Características Principais
- Implementação de navegador sem janela (windowless) para operação mais discreta
- Arquitetura modular com separação clara de responsabilidades
- Sistema de automação para detecção e resolução de hCaptcha
- Cliente API para serviços de resolução de captcha
- Processamento de imagens para facilitar a resolução
- Sistema de logging integrado

## Estrutura do Projeto
O projeto segue uma organização em camadas:

- **API** (`src/api/`):
  - `uCaptchaSonicClient.pas`: Cliente para comunicação com serviços de resolução de captcha

- **Tipos** (`src/types/`):
  - `uCaptchaTypes.pas`: Definições de tipos e estruturas para captchas

- **Automação** (`src/automation/`):
  - `uHCaptchaAutomation.pas`: Implementação da automação para resolver hCaptcha

- **Utilidades** (`src/utils/`):
  - `uImageProcessor.pas`: Processamento de imagens para captchas
  - `uLogger.pas`: Sistema de logging
  - `uConfig.pas`: Configurações da aplicação
  - `uWebView2Helper.pas`: Funções auxiliares para WebView2

- **Windowless Browser** (`WindowlessBrowser/`):
  - `uWindowlessBrowser.pas`: Implementação do navegador sem janela
  - `uDirectCompositionHost.pas`: Host para DirectComposition

## Como Compilar
1. Instale o Microsoft Edge WebView2 Runtime
2. Configure o ambiente Delphi com a biblioteca WebView4Delphi
3. Abra o arquivo `CaptchaSolver.dproj` no Delphi
4. Execute o Build (Ctrl+Shift+F9) ou Run (F9)

## Funcionalidades Técnicas
- **Navegador sem janela**: Utiliza DirectComposition para renderização sem criar uma janela visível
- **Automação hCaptcha**: Detecta e interage com elementos do hCaptcha
- **Captura de elementos**: Captura imagens e textos para análise
- **Comunicação com API**: Envia dados para serviços de resolução e processa respostas
- **Simulação de interações humanas**: Implementa movimentos e cliques naturais para evitar detecção

## Requisitos do Sistema
- Windows 10 ou superior
- Microsoft Edge WebView2 Runtime instalado
- Conexão com a Internet para comunicação com APIs
- Chave de API para serviço de resolução de captcha (opcional)

## Configuração
O arquivo `config.ini` na raiz do projeto permite configurar:
- Chave de API para serviço de captcha
- Tempos de espera e tentativas
- Opções de logging
- Comportamento da automação

## Diferenças em Relação ao PopularCaptchaTester
- Arquitetura mais modular e organizada
- Implementação de navegador sem janela (windowless)
- Foco exclusivo em hCaptcha (mais especializado)
- Melhor separação entre lógica de negócio e interface
- Sistema de configuração mais robusto 