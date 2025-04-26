# Modo Headless para Solução de Captcha

Este documento explica o modo headless (também conhecido como windowless) implementado neste projeto para solução de captchas hCaptcha usando a API CaptchaSonic.

## O que é o Modo Headless?

O modo headless permite executar o navegador WebView2 sem uma interface visível. Isso oferece várias vantagens:

1. **Automação completa**: Ideal para soluções que executam em servidores ou em segundo plano
2. **Menor consumo de recursos**: Não requer renderização visual completa
3. **Melhor desempenho**: Processos mais eficientes sem a necessidade de renderizar toda a interface
4. **Menos detecção**: Em alguns casos, pode evitar certas formas de detecção de automação

## Como Usar o Modo Headless

Para ativar o modo headless no aplicativo:

1. Marque a opção "Modo Headless (Windowless)" na interface do aplicativo
2. Reinicie a aplicação ou o navegador para que a alteração tenha efeito
3. A partir desse momento, todas as operações serão realizadas no modo headless

## Implementação Técnica

A implementação do modo headless utiliza a API DirectComposition do Windows para criar um navegador sem interface visível:

```delphi
if chkHeadless.Checked then
begin
  // Iniciar em modo headless (windowless)
  AddLog('Iniciando navegador em modo headless', 'INFO');
  FWVBrowser.CreateWindowlessBrowser(WVWindowParent1.Handle);
end
else
begin
  // Iniciar em modo normal
  AddLog('Iniciando navegador em modo normal', 'INFO');
  FWVBrowser.CreateBrowser(WVWindowParent1.Handle);
end;
```

## Funcionamento com CaptchaSonic

O fluxo de solução de captcha no modo headless é o seguinte:

1. O navegador carrega a página em modo invisível
2. O conteúdo é capturado usando MHTML
3. As imagens e dados do captcha são extraídos
4. Os dados são enviados para a API CaptchaSonic
5. A solução é aplicada usando simulação de eventos de mouse e teclado

## Considerações Importantes

Ao usar o modo headless:

- Algumas páginas podem detectar que o navegador está em modo headless
- Alguns recursos podem não funcionar exatamente como em modo normal
- A depuração pode ser mais complexa, pois a interface não é visível
- Em caso de problemas, recomenda-se desativar o modo headless para diagnóstico

## Requisitos Específicos para o Modo Headless

- Microsoft Edge WebView2 Runtime instalado
- O uso de DirectComposition API requer Windows 8 ou superior
- Pode ser necessário ter drivers gráficos atualizados

## Limitações

O modo headless possui algumas limitações:

- Não permite a visualização do processo de solução (útil para depuração)
- Algumas páginas podem bloquear o acesso via navegador headless
- Nem todas as funcionalidades do WebView são suportadas em modo headless

## Recomendações

- Use o modo headless para aplicações de produção ou automação em massa
- Use o modo normal para depuração e testes iniciais
- Faça testes comparativos entre os dois modos para verificar se há diferenças significativas no seu caso de uso específico 