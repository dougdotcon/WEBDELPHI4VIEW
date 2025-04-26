# Solucionador de Captcha Popular com API CaptchaSonic

Este projeto implementa uma solução para resolver automaticamente os desafios do hCaptcha utilizando o WebView4Delphi e a API CaptchaSonic.

## Recursos Implementados

- Integração com WebView4Delphi para renderizar e interagir com páginas web
- Suporte para execução em modo headless com o WebView2
- Integração com a API CaptchaSonic para resolver desafios hCaptcha
- Modos de solução:
  - objectClick: Clique em elementos específicos
  - objectDrag: Arraste de elementos na tela
  - objectClassify: Classificação de objetos

## Requisitos

- Delphi Rio ou superior
- WebView4Delphi instalado
- Conta na API CaptchaSonic (uma chave de API é necessária)
- Microsoft Edge WebView2 Runtime instalado

## Como Usar

1. **Configuração Inicial**
   - Insira sua chave de API do CaptchaSonic no campo "API Key"
   - Selecione o modo de solução no ComboBox "Modo Sonic"

2. **Navegação e Solução**
   - Digite a URL desejada no campo de endereço e clique em "Navegar"
   - Ao encontrar um desafio hCaptcha, clique em "Resolver com Sonic"
   - A aplicação irá:
     - Extrair as informações do captcha da página
     - Capturar as imagens usando MHTML
     - Enviar os dados para a API CaptchaSonic
     - Aplicar a solução recebida interagindo com o WebView

3. **Detalhes Técnicos**
   - A aplicação usa o método MHTML para capturar o conteúdo da página
   - A interação com o captcha é feita diretamente no WebView utilizando simulação de eventos do mouse
   - A API é chamada via HTTP com autenticação por chave de API

## Modos de Solução

- **objectClick**: Usado quando o captcha requer cliques em objetos específicos
- **objectDrag**: Usado quando o captcha requer arrastar e soltar elementos
- **objectClassify**: Usado quando o captcha requer classificação de objetos

## Informações de Depuração

O painel de log exibe informações detalhadas sobre o processo de solução, incluindo:
- Extração de dados
- Requisições à API
- Respostas recebidas
- Interações com o WebView

## Importante

- Esta aplicação utiliza a chave de API do CaptchaSonic que possui créditos limitados
- Cada solução de captcha consome créditos da sua conta
- Certifique-se de monitorar o saldo disponível em sua conta

## API CaptchaSonic

- Endereço: https://my.captchasonic.com/docs/product/popular
- API Key: Configure no campo apropriado
- Documentação completa disponível no site do CaptchaSonic 