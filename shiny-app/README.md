# RB Consultoria em Dados - App Shiny

Este app e a primeira versao da engine interativa do site.

## O que ele faz

- Calcula uma pontuacao orientativa de maturidade dos dados.
- Mostra recomendacoes conforme organizacao, qualidade, visualizacao e automacao.
- Gera um resumo para contato por e-mail.
- Permite baixar o resumo em `.txt`.

## Privacidade nesta versao

Esta versao nao grava respostas em banco, nao salva arquivos e nao armazena uploads. Tudo e calculado durante a sessao do usuario.

## Rodar localmente

No R:

```r
shiny::runApp("shiny-app")
```

Ou pelo terminal, a partir da raiz do projeto:

```powershell
& "C:\\Program Files\\R\\R-4.4.3\\bin\\Rscript.exe" -e "shiny::runApp('shiny-app', host='127.0.0.1', port=3838)"
```

## Publicar no shinyapps.io

1. Criar ou acessar uma conta em https://www.shinyapps.io/
2. Instalar o pacote `rsconnect`, se necessario:

```r
install.packages("rsconnect")
```

3. Configurar a conta com token e secret do painel do shinyapps.io:

```r
rsconnect::setAccountInfo(
  name = "SEU_USUARIO",
  token = "SEU_TOKEN",
  secret = "SEU_SECRET"
)
```

4. Publicar:

```r
rsconnect::deployApp("shiny-app", appName = "rb-diagnostico-dados")
```
