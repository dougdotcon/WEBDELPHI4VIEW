# Solucionador de hCaptcha em Delphi

Este projeto implementa um solucionador automatizado de desafios hCaptcha usando Delphi e a API do CaptchaSonic.

## Requisitos

- Delphi 10.4 ou superior
- Windows 10/11
- Microsoft Edge WebView2 Runtime
- Conta CaptchaSonic com créditos disponíveis
- Componentes VCL do Delphi
- Pacotes de runtime do Delphi

## Estrutura do Projeto e Arquivos Necessários

```
CaptchaSolver/
├── src/
│   ├── api/
│   │   └── uCaptchaSonicClient.pas
│   ├── types/
│   │   └── uCaptchaTypes.pas
│   ├── automation/
│   │   └── uHCaptchaAutomation.pas
│   └── utils/
│       ├── uImageProcessor.pas
│       ├── uLogger.pas
│       ├── uConfig.pas
│       └── uWebView2Helper.pas
├── WindowlessBrowser/
│   ├── uWindowlessBrowser.pas/dfm
│   └── uDirectCompositionHost.pas
├── WebView2.pas
├── WebView2.Loader.pas
├── config.ini
├── CaptchaSolver.dpr
├── CaptchaSolver.dproj
└── CaptchaSolver.res (gerado na compilação)
```

### Arquivos Essenciais
Todos os arquivos listados acima são necessários para a compilação. O arquivo `CaptchaSolver.res` será gerado automaticamente durante a primeira compilação.

### Dependências Externas
- Microsoft Edge WebView2 Runtime (deve estar instalado no sistema)
- Componentes VCL do Delphi
- Pacotes de runtime do Delphi
- Bibliotecas para manipulação de imagens (se necessário)
- Componentes para comunicação HTTP

## Configuração

1. Instale o Microsoft Edge WebView2 Runtime
2. Certifique-se de que todas as DLLs necessárias estão no PATH do sistema ou na pasta do executável
3. Obtenha uma chave de API do CaptchaSonic
4. Configure o arquivo `config.ini`:
   ```ini
   [CaptchaSonic]
   APIKey=sua_chave_api_aqui
   BaseURL=https://my.captchasonic.com/api/v1
   TimeoutMS=30000
   RetryCount=3
   DelayBetweenActionsMS=200
   RandomDelayRangeMS=300

   [Logging]
   LogFile=logs\captcha_solver.log
   LogLevel=1
   ```

## Compilação

1. Verifique se todos os arquivos necessários estão presentes
2. Certifique-se de que todas as dependências estão instaladas
3. Abra o projeto `CaptchaSolver.dproj` no Delphi
4. Compile o projeto (Shift+F9)
5. O executável será gerado em `Win32\Debug\CaptchaSolver.exe`

## Uso

1. Execute o programa
2. Insira a URL do site que contém o hCaptcha
3. Clique em "Iniciar" para começar o processo de solução
4. O programa irá:
   - Detectar o hCaptcha na página
   - Extrair a chave do site
   - Enviar o desafio para o CaptchaSonic
   - Aplicar a solução automaticamente

## Logs

Os logs são salvos em `logs\captcha_solver.log` e podem ser visualizados no programa.
Níveis de log:
- 0: Apenas erros
- 1: Informações importantes
- 2: Informações detalhadas de debug

## Solução de Problemas

Se encontrar problemas na compilação:
1. Verifique se todos os arquivos listados estão presentes
2. Confirme se o WebView2 Runtime está instalado
3. Verifique se todas as dependências do VCL estão instaladas
4. Certifique-se de que o arquivo `config.ini` está configurado corretamente

## Limitações

- Funciona apenas com hCaptcha
- Requer conexão com a internet
- Necessita de créditos no CaptchaSonic
- Pode ser detectado como automação por alguns sites

## Contribuição

Sinta-se à vontade para contribuir com o projeto através de pull requests.
Por favor, mantenha o código organizado e documente as mudanças adequadamente.

## Licença

Este projeto está licenciado sob a MIT License.