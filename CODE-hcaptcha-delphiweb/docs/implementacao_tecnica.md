# Implementação Técnica para Solução de hCaptcha com WebView4Delphi

## Requisitos do Projeto

Com base no arquivo README.md e nas imagens analisadas, o projeto requer:

1. Utilização do projeto WebView4Delphi (https://github.com/salvadordf/Webview4delphi)
2. Integração com a API CaptchaSonic (https://my.captchasonic.com/docs/product/popular)
3. Implementação dos métodos objectClick, objectDrag e objectClassify
4. Utilização do WebView2 em modo headless
5. Minimização do uso de JavaScript
6. Desenvolvimento em Delphi Rio

## Estrutura do Desafio hCaptcha

Os desafios hCaptcha observados nas imagens seguem um padrão consistente:

- Container principal com classe `challenge-container`
- Texto de instrução em elemento com classe `prompt-text`
- Grade de imagens representadas como elementos `div` com classe `task`
- Imagem de referência no canto superior direito
- Botão "Pular" na parte inferior

## Detalhes da Solução Técnica

### 1. Captura do DOM usando MHTML

```pascal
procedure TMainForm.CapturarMHTML;
var
  MHTMLContent: string;
begin
  // Método para capturar o conteúdo MHTML da página atual
  FWebView.GetExecutedJavascript('document.location.href', 
    procedure(const aValue : string)
    begin
      // Obter a URL atual para referência
      FCurrentURL := aValue;
      
      // Capturar o MHTML do documento atual
      FWebView.GetMHTML(
        procedure(const aMHTML : string)
        begin
          MHTMLContent := aMHTML;
          ProcessarMHTML(MHTMLContent);
        end
      );
    end
  );
end;
```

### 2. Extração de Imagens do Desafio

```pascal
procedure TMainForm.ExtrairImagensDesafio(const aMHTML: string);
var
  ImagemReferencia: TMemoryStream;
  ImagensGrid: TArray<TMemoryStream>;
  i: Integer;
begin
  // Extrair imagem de referência do MHTML
  ImagemReferencia := ExtrairImagemPorClasse(aMHTML, 'challenge-example');
  
  // Extrair imagens da grade do MHTML
  SetLength(ImagensGrid, 9);
  for i := 0 to 8 do
  begin
    ImagensGrid[i] := ExtrairImagemPorIndice(aMHTML, 'task', i);
  end;
  
  // Enviar para processamento na API CaptchaSonic
  ProcessarImagensComAPI(ImagemReferencia, ImagensGrid);
end;
```

### 3. Integração com CaptchaSonic API

```pascal
procedure TMainForm.ProcessarImagensComAPI(
  const aImagemReferencia: TMemoryStream; 
  const aImagensGrid: TArray<TMemoryStream>);
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  JSONBody: TJSONObject;
  ImagensBase64: TJSONArray;
  i: Integer;
begin
  // Configurar cliente REST
  Client := TRESTClient.Create('https://my.captchasonic.com/api/v1');
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);
  
  try
    Request.Client := Client;
    Request.Response := Response;
    Request.Method := TRESTRequestMethod.rmPOST;
    Request.Resource := 'objectClassify';
    
    // Definir cabeçalhos
    Request.Params.AddItem('Authorization', 'Bearer sonic_97300fd03e35a8a9442479637f96e42c919e', 
      TRESTRequestParameterKind.pkHTTPHEADER);
    
    // Criar corpo da requisição
    JSONBody := TJSONObject.Create;
    JSONBody.AddPair('reference_image', MemoryStreamToBase64(aImagemReferencia));
    
    // Adicionar imagens da grade
    ImagensBase64 := TJSONArray.Create;
    for i := 0 to Length(aImagensGrid) - 1 do
      ImagensBase64.Add(MemoryStreamToBase64(aImagensGrid[i]));
    
    JSONBody.AddPair('images', ImagensBase64);
    
    Request.Body.JSONObject := JSONBody;
    Request.Execute;
    
    if Response.StatusCode = 200 then
      ProcessarRespostaCaptchaSonic(Response.Content)
    else
      ShowMessage('Erro na requisição: ' + Response.StatusCode.ToString);
      
  finally
    // Liberar recursos
    Client.Free;
    Request.Free;
    Response.Free;
  end;
end;
```

### 4. Interação com o hCaptcha via WebView4Delphi

```pascal
procedure TMainForm.ClicarImagensSelecionadas(const aIndices: TArray<Integer>);
var
  i: Integer;
  ElementID: string;
begin
  for i := 0 to Length(aIndices) - 1 do
  begin
    // Construir seletor para localizar o elemento
    ElementID := Format('div.task[aria-label="Imagem do desafio %d"]', [aIndices[i] + 1]);
    
    // Localizar elemento usando seletor CSS nativo
    FWebView.ExecuteQuery(ElementID, 
      procedure(const aSuccess : boolean; const aDocument : ICoreWebView2DOMXPathQueryResult)
      var
        Element: ICoreWebView2DOMElement;
      begin
        if aSuccess and (aDocument.GetResultCount > 0) then
        begin
          Element := aDocument.GetResultElements(0);
          
          // Obter posição e tamanho do elemento
          Element.GetBoundingClientRect(
            procedure(const aSuccess : boolean; const aRect : TRectF)
            begin
              if aSuccess then
              begin
                // Calcular centro do elemento para clique
                ClickAtPosition(
                  Round(aRect.Left + (aRect.Width / 2)),
                  Round(aRect.Top + (aRect.Height / 2))
                );
              end;
            end
          );
        end;
      end
    );
    
    // Adicionar pequeno atraso para simular comportamento humano
    Sleep(Random(300) + 200);
  end;
end;

procedure TMainForm.ClickAtPosition(const aX, aY: Integer);
begin
  // Usar método nativo para clique em posição específica
  FWebView.SimulateMouseClick(aX, aY, mcLeft);
end;
```

## Fluxo Completo da Solução

1. Inicializar WebView4Delphi em modo headless
2. Navegar até a página com o desafio hCaptcha
3. Aguardar carregamento do desafio
4. Capturar o DOM usando MHTML
5. Extrair a imagem de referência e as imagens da grade
6. Enviar para API CaptchaSonic (objectClassify)
7. Receber resposta com índices das imagens correspondentes
8. Interagir com o hCaptcha clicando nas imagens identificadas
9. Verificar conclusão do desafio
10. Gerenciar erros e tentativas

## Considerações de Desempenho e Segurança

1. **Detecção de Bot**: Implementar atrasos aleatórios e padrões de movimento do mouse para simular comportamento humano
2. **Gerenciamento de Memória**: Liberar adequadamente os recursos, especialmente streams de imagem
3. **Tratamento de Erros**: Implementar mecanismos robustos para lidar com falhas na API ou no reconhecimento
4. **Segurança da API Key**: Armazenar a chave da API de forma segura, evitando exposição no código-fonte 