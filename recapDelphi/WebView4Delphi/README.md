# PopularCaptchaTester

## Versão do Delphi Compatível
Este projeto foi desenvolvido utilizando Delphi versão 19.5 (Delphi 11 Alexandria) e é compatível com:
- Delphi 11 Alexandria (principal)
- Versões anteriores que suportem WebView4Delphi (recomendado Delphi 10.4 ou superior)

## Como Compilar o Projeto

### Pré-requisitos
1. **Ambiente de Desenvolvimento**:
   - Delphi instalado (recomendado Delphi 11 Alexandria)
   - Microsoft Edge WebView2 Runtime instalado
   - WebView4Delphi instalado e configurado

2. **Dependências**:
   - WebView4Delphi (biblioteca principal)
   - Microsoft Edge WebView2 Runtime
   - VCL (Visual Component Library)

### Passos para Compilação

1. **Preparação do Ambiente**:
   - Instale o Microsoft Edge WebView2 Runtime
   - Instale a biblioteca WebView4Delphi no seu Delphi

2. **Configuração do Projeto**:
   - Abra o projeto `PopularCaptchaTester.dproj` no Delphi
   - Verifique se o WebView4Delphi está corretamente configurado no Library Path do Delphi

3. **Compilação**:
   - Selecione a plataforma desejada (Win32, Android64 ou Linux64)
   - Execute Build (Ctrl+Shift+F9) ou Run (F9)

## O que Falta para Compilar

1. **Componentes Necessários**:
   - WebView4Delphi (necessário instalar)
   - Microsoft Edge WebView2 Runtime (necessário instalar)

2. **Configurações Adicionais**:
   - Configurar uma chave de API válida para o serviço de resolução de captcha
   - Verificar se os paths do WebView4Delphi estão corretamente configurados no Delphi
   - Garantir que todas as dependências do VCL estejam disponíveis

## Como Usar a Biblioteca Web4Delphi neste Projeto

### 1. Instalação do Web4Delphi

1. **Download e Instalação**:
   ```
   git clone https://github.com/salvadordf/WebView4Delphi.git
   ```
   - Adicione o diretório ao Library Path do Delphi
   - Compile e instale os pacotes necessários

2. **Configuração do Runtime**:
   - Instale o Microsoft Edge WebView2 Runtime
   - Link: https://developer.microsoft.com/en-us/microsoft-edge/webview2/

### 2. Implementação no Projeto

O projeto já está configurado para usar o Web4Delphi. Os principais pontos de integração são:

1. **No Formulário Principal (uMainForm)**:
   - Componente WebView para renderização do captcha
   - Controles de navegação e manipulação web

2. **No Solucionador de Captcha (uPopularCaptchaSolver)**:
   - Manipulação de frames
   - Injeção de scripts
   - Extração de dados MHTML

### 3. Funcionalidades Principais do Web4Delphi Utilizadas

- Navegação web
- Manipulação de frames
- Injeção de JavaScript
- Extração de conteúdo MHTML
- Manipulação de cookies
- Eventos de navegação

### 4. Recursos Implementados

- Suporte a múltiplos tipos de captcha
- Detecção automática
- Interface de usuário integrada
- Sistema de log
- Controles de navegação

## Observações Importantes

1. **Segurança**:
   - Não exponha sua chave de API
   - Implemente rate limiting
   - Valide entradas de dados

2. **Performance**:
   - O WebView2 Runtime é necessário
   - Conexão com internet é obrigatória
   - Recursos de sistema adequados são necessários

3. **Limitações**:
   - Funciona apenas em Windows
   - Requer WebView2 Runtime
   - Dependente de conexão internet

## Suporte e Documentação Adicional

- Documentação detalhada disponível em `/docs`
- Guia de uso em `/docs/how-to-use.md`
- Documentação técnica em `/docs/doc.md`

Para mais informações sobre o Web4Delphi, consulte:
- [Repositório oficial do Web4Delphi](https://github.com/salvadordf/WebView4Delphi)
- [Documentação do WebView2 Runtime](https://learn.microsoft.com/en-us/microsoft-edge/webview2/)