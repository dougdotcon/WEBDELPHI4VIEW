# Documentação do Solucionador de hCaptcha em Delphi

## 1. Visão Geral

Este é um projeto Delphi que implementa um solucionador automatizado de desafios hCaptcha utilizando a API do CaptchaSonic e o Microsoft Edge WebView2. O projeto foi desenvolvido para automatizar a resolução de captchas em aplicações Windows.

## 2. Requisitos do Sistema

### 2.1 Software Necessário
- Delphi 10.4 ou superior
- Windows 10/11
- Microsoft Edge WebView2 Runtime

### 2.2 Requisitos de API
- Conta ativa no CaptchaSonic
- Créditos disponíveis para solução de captchas
- Chave de API válida

## 3. Estrutura do Projeto

```
CaptchaSolver/
├── src/
│   ├── forms/          # Interfaces gráficas
│   ├── api/            # Integração com CaptchaSonic
│   ├── types/          # Definições de tipos
│   ├── automation/     # Automação do hCaptcha
│   └── utils/          # Utilitários diversos
├── WindowlessBrowser/  # Implementação do browser
├── logs/              # Arquivos de log
├── config.ini         # Configurações
└── CaptchaSolver.dpr  # Arquivo principal
```

## 4. Configuração

### 4.1 Instalação
1. Clone o repositório
2. Instale o Microsoft Edge WebView2 Runtime
3. Abra o projeto no Delphi
4. Compile o projeto (Shift+F9)

### 4.2 Configuração do arquivo config.ini
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

#### Parâmetros de Configuração:
- **APIKey**: Sua chave de API do CaptchaSonic
- **BaseURL**: URL base da API
- **TimeoutMS**: Tempo máximo de espera em milissegundos
- **RetryCount**: Número de tentativas em caso de falha
- **DelayBetweenActionsMS**: Delay entre ações
- **RandomDelayRangeMS**: Variação aleatória do delay
- **LogLevel**: Nível de detalhamento dos logs (0-2)

## 5. Como Usar

### 5.1 Modo GUI
1. Execute o `CaptchaSolver.exe`
2. Insira a URL do site com hCaptcha
3. Clique em "Iniciar"
4. Acompanhe o progresso na interface

### 5.2 Monitoramento
- Os logs são salvos em `logs\captcha_solver.log`
- Níveis de log disponíveis:
  - 0: Apenas erros
  - 1: Informações importantes
  - 2: Debug detalhado

## 6. Componentes Principais

### 6.1 API Client (uCaptchaSonicClient)
- Gerencia comunicação com CaptchaSonic
- Envia requisições de solução
- Processa respostas

### 6.2 Automação (uHCaptchaAutomation)
- Detecta presença do hCaptcha
- Extrai dados necessários
- Aplica soluções automaticamente

### 6.3 Processamento de Imagem (uImageProcessor)
- Processa imagens do captcha
- Prepara dados para envio à API

### 6.4 Logger (uLogger)
- Registra eventos do sistema
- Gerencia níveis de log
- Rotação de arquivos de log

## 7. Boas Práticas

### 7.1 Segurança
- Nunca compartilhe sua chave de API
- Mantenha o WebView2 atualizado
- Use proxy se necessário
- Implemente rate limiting

### 7.2 Performance
- Ajuste os delays conforme necessidade
- Monitore uso de recursos
- Limpe logs regularmente

## 8. Troubleshooting

### 8.1 Problemas Comuns
1. **Falha na Detecção**
   - Verifique se o site está acessível
   - Confirme se o hCaptcha está visível
   - Aumente timeout se necessário

2. **Erros de API**
   - Verifique sua chave de API
   - Confirme saldo de créditos
   - Verifique conectividade

3. **Problemas de Performance**
   - Ajuste os delays
   - Verifique uso de memória
   - Monitore logs de erro

## 9. Limitações

- Funciona apenas com hCaptcha
- Requer conexão à internet
- Depende de créditos no CaptchaSonic
- Pode ser detectado como automação

## 10. Contribuição

1. Fork o repositório
2. Crie uma branch para sua feature
3. Commit suas mudanças
4. Push para a branch
5. Crie um Pull Request

## 11. Suporte

- Verifique a documentação em `/docs`
- Consulte os logs em `/logs`
- Reporte issues no GitHub
- Mantenha o sistema atualizado

## 12. Licença

Este projeto está licenciado sob a MIT License. Consulte o arquivo LICENSE para mais detalhes.

---

Esta documentação fornece uma visão abrangente do projeto e suas funcionalidades. Para questões específicas ou suporte adicional, consulte a documentação técnica em `/docs` ou entre em contato com a equipe de desenvolvimento.
