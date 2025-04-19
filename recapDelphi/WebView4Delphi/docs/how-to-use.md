# Documentação do PopularCaptchaTester

## Descrição
O PopularCaptchaTester é uma aplicação Delphi desenvolvida para testar e resolver automaticamente captchas hCaptcha utilizando um navegador web embutido (WebView2).

## Requisitos do Sistema
- Windows 10/11
- Microsoft Edge WebView2 Runtime instalado
- Delphi (versão compatível com VCL)
- Chave de API válida para o serviço de resolução de captcha

## Componentes Principais
1. **uMainForm**: Interface principal do usuário
2. **uPopularCaptchaSolver**: Núcleo do sistema de resolução de captcha

## Funcionalidades Principais

### 1. Interface do Usuário
- Campo para URL do captcha
- Campo para chave de API
- Área de visualização do navegador
- Log de eventos
- Controles de configuração

### 2. Configurações Disponíveis
- **Auto Solve**: Resolução automática de captchas
- **Auto Open**: Abertura automática do captcha
- **English**: Forçar idioma inglês
- **Always Solve**: Sempre tentar resolver
- **Detection Method**: Método de detecção do captcha

### 3. Tipos de Captcha Suportados
- Widget padrão
- Seleção múltipla
- Grade (Grid)
- Bbox
- BboxDD

## Como Usar

### 1. Configuração Inicial
1. Abra o aplicativo
2. Insira sua chave de API no campo "API Key"
3. Selecione as configurações desejadas nos checkboxes

### 2. Carregando um Captcha
Existem duas formas:
- Digite a URL do captcha e clique em "Navigate"
- Use o botão "Load Sample" para carregar exemplos pré-definidos

### 3. Resolvendo Captchas
- Clique em "Solve" para iniciar a resolução
- O progresso e resultados serão exibidos na área de log
- Use "Stop" para interromper o processo

### 4. Métodos de Detecção
Selecione no combobox:
- Automático (recomendado)
- Outros métodos específicos disponíveis

## Monitoramento e Debug
- A área de log mostra informações detalhadas do processo
- O status bar indica o estado atual da operação
- Diferentes níveis de log são identificados por cores:
  - Debug: Informações detalhadas
  - Info: Informações gerais
  - Warn: Avisos
  - Error: Erros

## Recursos Avançados
- Suporte a múltiplos frames
- Extração de MHTML
- Injeção de scripts
- Simulação de interações do mouse
- Detecção automática do tipo de captcha

## Solução de Problemas

### Problemas Comuns:
1. **Captcha não carrega**:
   - Verifique a conexão com internet
   - Confirme se o WebView2 Runtime está instalado

2. **Falha na resolução**:
   - Verifique se a chave API é válida
   - Tente atualizar o frame com "Refresh Frame"
   - Verifique o log para mensagens de erro específicas

3. **Navegador não inicializa**:
   - Reinstale o WebView2 Runtime
   - Verifique permissões do sistema

## Notas de Segurança
- Mantenha sua chave API em segurança
- Não compartilhe logs que contenham sua chave API
- Use o sistema de acordo com os termos de serviço do hCaptcha

## Limitações
- Funciona apenas em ambiente Windows
- Requer conexão com internet
- Alguns tipos de captcha podem não ser suportados
- Taxa de sucesso depende da qualidade da chave API

## Suporte Técnico
Para problemas técnicos, verifique:
1. Log de eventos no aplicativo
2. Configurações do WebView2
3. Conexão com internet
4. Validade da chave API

Esta documentação fornece uma visão geral completa do sistema. Para casos específicos ou dúvidas adicionais, consulte o código fonte ou entre em contato com o desenvolvedor.
