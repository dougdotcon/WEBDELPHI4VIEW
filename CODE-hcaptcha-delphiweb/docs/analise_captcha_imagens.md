# Análise de Desafio hCaptcha

## Visão Geral
As imagens fornecidas mostram um desafio hCaptcha que solicita ao usuário identificar partes específicas do corpo de animais. Este tipo de desafio é comum em sistemas anti-bot e requer interação do usuário para verificar que é um humano.

## Elementos do Desafio

### Instrução Principal
O desafio apresenta a instrução: "Clique nas imagens do animal ao qual pertence esta parte do corpo". Abaixo da instrução, há uma orientação adicional: "Se não houver nenhum, clique em Pular".

### Interface do Desafio
- **Imagem de Referência**: No canto superior direito, existe uma imagem de referência que mostra a parte específica do corpo animal que deve ser identificada.
- **Grade de Imagens**: Na parte principal, há uma grade 3x3 contendo 9 imagens de diferentes animais.
- **Botão "Pular"**: No canto inferior direito, há um botão para pular o desafio se nenhuma das imagens corresponder à parte do corpo solicitada.

### Estrutura HTML
A análise do código HTML revela os seguintes elementos importantes:

1. **Container Principal**: `<div class="challenge-container">`
2. **Cabeçalho do Desafio**: `<div class="challenge-header">`
3. **Texto da Instrução**: `<div class="prompt-text">` contendo o texto do desafio
4. **Grade de Imagens**: Classes como `<div class="task" tabindex="0">` para cada célula da grade
5. **Botões de Interação**: Elementos para selecionar ou pular

### Elementos para Automação
Elementos importantes para automação via WebView4Delphi:

1. **Seleção de Imagens**: Cada imagem na grade possui atributos como:
   ```html
   <div class="task" tabindex="0" role="button" aria-label="Imagem do desafio 1" aria-pressed="false">
   ```

2. **Botão Pular**: 
   ```html
   <div class="skip" style="width: 250px; color: rgb(255, 255, 255); font-size: 14px; vertical-align: bottom;">
   ```

3. **Textos de Instrução**:
   ```html
   <h2 class="prompt-text">Clique nas imagens do animal ao qual pertence esta parte do corpo</h2>
   ```

## Abordagem para Automação com WebView4Delphi e CaptchaSonic

### Estratégia de Implementação
1. **Captura do Estado do Desafio**:
   - Utilizar o método MHTML para capturar o DOM completo
   - Identificar a imagem de referência e as imagens na grade

2. **Processamento com CaptchaSonic**:
   - Enviar a imagem de referência e a grade para a API objectClassify
   - Receber as coordenadas ou índices das imagens correspondentes

3. **Interação com o Desafio**:
   - Utilizar os comandos nativos do WebView4Delphi para clicar nas imagens identificadas
   - Evitar uso de JavaScript, conforme recomendado no readme

4. **Verificação de Conclusão**:
   - Monitorar mudanças na interface para verificar se o desafio foi concluído com sucesso

### Considerações Técnicas
- Manter o processamento em modo headless
- Evitar detecção por comportamento não-humano
- Gerenciar timeouts e falhas de reconhecimento

## Implementação com WebView4Delphi e CaptchaSonic API

A implementação deve seguir um padrão que permita:
1. Inicializar o WebView4Delphi em modo headless
2. Carregar a página com o desafio hCaptcha
3. Capturar o estado atual do desafio usando MHTML
4. Processar as imagens com a API CaptchaSonic (objectClassify)
5. Interagir com os elementos identificados usando comandos nativos
6. Verificar o resultado e gerenciar exceções 