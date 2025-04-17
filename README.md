# WEBDELPHI - Projetos de Automação de Captcha

Este repositório contém dois projetos Delphi relacionados à automação e resolução de captchas utilizando a biblioteca WebView4Delphi. Ambos os projetos foram recentemente compilados com sucesso.

## Projetos Incluídos

### 1. [PopularCaptchaTester (SOURCE-web4delphi-dev-teste)](./README-SOURCE.md)

Um aplicativo para testar e resolver automaticamente diferentes tipos de captchas populares. Este projeto oferece uma interface visual completa para interagir com captchas e possui suporte a vários tipos de desafios.

**Principais características:**
- Interface de usuário completa com navegador integrado
- Suporte a múltiplos tipos de captcha
- Sistema de logs para monitoramento
- Capacidade de automação de resolução

[Leia mais sobre o PopularCaptchaTester](./README-SOURCE.md)

### 2. [CaptchaSolver (CODE-hcaptcha-delphiweb)](./README-CODE.md)

Uma implementação mais especializada focada na resolução de hCaptcha usando um navegador sem janela (windowless browser). Este projeto adota uma arquitetura mais modular e organizada.

**Principais características:**
- Implementação de navegador sem janela (windowless)
- Arquitetura modular e bem estruturada
- Foco específico em hCaptcha
- Sistema de automação avançado

[Leia mais sobre o CaptchaSolver](./README-CODE.md)

## Requisitos Comuns

Ambos os projetos compartilham alguns requisitos básicos:

- Sistema operacional Windows 10 ou superior
- Microsoft Edge WebView2 Runtime instalado
- Delphi instalado (recomendado Delphi 11 ou superior)
- Conexão com a Internet para resolução online de captchas

## Tecnologia Principal

Ambos os projetos utilizam a biblioteca **WebView4Delphi**, que permite incorporar o navegador Microsoft Edge WebView2 em aplicações Delphi. Esta biblioteca fornece uma maneira poderosa de interagir com conteúdo web dentro de aplicações desktop.

## Como Utilizar

1. Clone ou baixe este repositório
2. Instale o Microsoft Edge WebView2 Runtime
3. Configure o ambiente Delphi com a biblioteca WebView4Delphi
4. Escolha o projeto que melhor atende às suas necessidades
5. Abra o arquivo de projeto (.dproj) correspondente no Delphi
6. Compile e execute o projeto

## Comparação entre os Projetos

| Característica | PopularCaptchaTester | CaptchaSolver |
|----------------|----------------------|---------------|
| Interface | GUI completa | Navegador sem janela |
| Tipos de captcha | Múltiplos tipos | Foco em hCaptcha |
| Arquitetura | Monolítica | Modular |
| Complexidade | Média | Alta |
| Flexibilidade | Ampla | Específica |

Escolha o projeto mais adequado às suas necessidades específicas de automação de captcha. 