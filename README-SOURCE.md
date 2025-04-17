# PopularCaptchaTester (SOURCE-web4delphi-dev-teste)

## Visão Geral
O projeto **PopularCaptchaTester** é uma aplicação desenvolvida em Delphi que permite testar e resolver automaticamente captchas populares. Ele utiliza a biblioteca WebView4Delphi para integrar um navegador baseado no Microsoft Edge WebView2 dentro da aplicação.

## Tecnologias Utilizadas
- **Delphi**: O projeto foi desenvolvido e testado principalmente no Delphi 11 Alexandria
- **WebView4Delphi**: Biblioteca principal para incorporar o navegador
- **Microsoft Edge WebView2 Runtime**: Mecanismo necessário para renderização web
- **VCL (Visual Component Library)**: Framework de interface gráfica

## Funcionalidades Principais
- Detecção automática de diferentes tipos de captchas
- Resolução automática de captchas
- Interface de usuário integrada e amigável
- Sistema de logs para monitoramento
- Suporte a múltiplos tipos de captcha (Widget, MultiSelect, Grid, Bbox, BboxDD)
- Manipulação avançada de frames e injeção de scripts JavaScript

## Requisitos do Sistema
- Windows 10 ou 11 (64 bits recomendado)
- Delphi instalado (recomendado Delphi 11 Alexandria)
- Microsoft Edge WebView2 Runtime
- Conexão com a Internet para resolução de captchas online

## Estrutura do Projeto
- **uMainForm.pas/dfm**: Formulário principal da aplicação
- **uPopularCaptchaSolver.pas**: Implementação do solucionador de captchas
- **PopularCaptchaTester.dpr**: Arquivo de projeto principal

## Como Compilar
1. Instale o Microsoft Edge WebView2 Runtime
2. Configure o ambiente Delphi com a biblioteca WebView4Delphi
3. Abra o arquivo `PopularCaptchaTester.dproj` no Delphi
4. Execute o Build (Ctrl+Shift+F9) ou Run (F9)

## Uso da Aplicação
1. Insira uma URL contendo um captcha no campo de navegação
2. Utilize os controles para carregar e resolver o captcha
3. Configure as opções de acordo com suas preferências:
   - Auto Solve: Resolve automaticamente ao detectar captcha
   - Auto Open: Abre automaticamente ao detectar captcha
   - English Language: Força o idioma inglês para o captcha
   - Always Solve: Tenta resolver mesmo em casos incertos

## Implementação Técnica
O projeto utiliza uma abordagem sofisticada para lidar com captchas:
- Injeção de JavaScript para manipulação de elementos da página
- Captura de conteúdo MHTML para análise offline
- Detecção de frames e elementos específicos
- Simulação de interações humanas para evitar bloqueios
- Integração com APIs de serviços de resolução de captcha

## Limitações
- Funciona apenas em sistemas Windows
- Requer WebView2 Runtime
- Dependente de conexão com a Internet

## Documentação Adicional
Consulte o arquivo `GUIA_INSTALACAO_E_USO.md` para instruções detalhadas sobre instalação e uso do sistema. 