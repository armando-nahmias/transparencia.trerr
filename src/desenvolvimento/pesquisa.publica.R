consulta.publica <- list()

dados <- data.frame(
          pa = character(),
          documento = character(),
          endereco_doc = character(),
          unidade = character(),
          data = integer(),
          texto = character(),
          autoridade = character(),
          cargo = character(),
          ano = integer()
)

if (file.exists('dados/dados.consolidados.rds')) {
          dados.anteriores <- readRDS('dados/dados.consolidados.rds')
} else {
          dados.anteriores <- dados
}

if (file.exists('dados/data.consulta.rds')) {
          data.consulta <- readRDS('dados/data.consulta.rds')
}

## Definindo parâmetros

# Definindo o Tipo de Documento a ser buscado
atos <- list(
          # ARP = '179',
          CartaContrato = '50123',
          # DOD = '50193',
          # DODTI = '50007',
          # EditalLicita = '438',
          # ETP = '50072',
          TermoAditivo = '14',
          # TR = '736',
          # Portaria = '50001',
          Contrato = '100'
)

# Definindo as datas inicial e final da busca

if (exists('data.consulta')) {
          data.inicial <- data.consulta + 1
          
} else {
          data.inicial <- as.Date('2024-01-01')
}

data.final <- data.consulta <- Sys.Date()


# Definindo o hash da pesquisa
resumo = "ca0ea880082cda72477eece3c825d7ce"


## Busca dos dados dos documentos
for (ato in as.list(atos)) {
          inicio = 0
          print(paste0('Raspando ', names(ato)))
          estado <- 'pesquisa'
          while (estado == 'pesquisa') {
                    print(paste0('Buscando pagina ', inicio))
                    resposta <-
                              buscar.pagina(inicio, resumo, ato, data.inicial, data.final)
                    pagina <- rvest::read_html(resposta)
                    print(resposta$status_code)
                    # dado <- obter.dados(pagina)
                    dados.tabela <- obter.dados.tabela(pagina)
                    dados <- rbind(dados, dados.tabela)
                    
                    if (length(dados$pa) > 20) {
                              #if (length(dado$pa) != 10) {
                              estado <- 'terminou'
                    }
                    
                    Sys.sleep(.5)
                    
                    inicio = inicio + 10
          }
}


dados.novos <- dados[!dados$endereco_doc %in% dados.anteriores$endereco_doc, ]

print(paste0('Foram localizados ', nrow(dados.novos), ' novos documentos nesta consulta.'))

if (nrow(dados.novos) != 0) {
          for (item in 1:length(dados.novos$pa)) {
                    if (is.na(dados.novos$texto[item])) {
                              print(paste0('Consultando ', dados.novos$documento[item]))
                              dados.novos$texto[item] <- extrair.texto(item)
                    }
          }
          
          for (item in 1:length(dados.novos$pa)) {
                    if (is.na(dados.novos$autoridade[item])) {
                              print(paste0('Consultando ', dados.novos$documento[item]))
                              dados.novos$autoridade[item] <- extrair.autoridade(item)[, 2]
                              dados.novos$cargo[item] <- extrair.autoridade(item)[, 3]
                    }
          }
          
          dados.novos$ano <- lubridate::year(lubridate::ymd(dados.novos$data))
          
          if (exists('dados.anteriores')) {
                    dados.consolidados <- rbind(dados.novos, dados.anteriores)
          } else {
                    dados.consolidados <- dados.novos
          }
          saveRDS(dados.consolidados, file = 'dados/dados.consolidados.rds')
          
}



saveRDS(data.consulta, file = 'dados/data.consulta.rds')


# funcoes -----------------------------------------------------------------

## Funções

buscar.pagina <-  function(inicio, resumo, ato, data.inicial, data.final) {
          require(httr)
          
          cookies = c(
                    `__utma` = "68700116.884304183.1678453746.1678453746.1678453746.1",
                    `__utmz` = "68700116.1678453746.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none)",
                    `TS01824ef7` = "0103a0ceaeb9fef5f80eb6488a5931bfae7d3d1f2148cda277397d25f37dfa6b97c8f478477dc9bf28837b7b688f31e33ab9e2da18d1198e8b19d2b81a4c34e3feb0f1ceb4",
                    `PHPSESSID` = "pj76lvr6cl57hir8u540ji06lb",
                    `TRE-RR_SEI__menu_mostrar` = "N"
          )
          
          headers = c(
                    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:109.0) Gecko/20100101 Firefox/117.0",
                    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
                    `Accept-Language` = "pt-BR,pt;q=0.8,en-US;q=0.5,en;q=0.3",
                    `Accept-Encoding` = "gzip, deflate, br",
                    `Referer` = "https://sei.tre-rr.jus.br/",
                    `Content-Type` = "application/x-www-form-urlencoded",
                    `Origin` = "https://sei.tre-rr.jus.br",
                    `Connection` = "keep-alive",
                    `Upgrade-Insecure-Requests` = "1",
                    `Sec-Fetch-Dest` = "document",
                    `Sec-Fetch-Mode` = "navigate",
                    `Sec-Fetch-Site` = "same-origin",
                    `Sec-Fetch-User` = "?1"
          )
          
          params = list(
                    `acao_externa` = "protocolo_pesquisar",
                    `acao_origem_externa` = "protocolo_pesquisar_paginado",
                    `inicio` = inicio,
                    `id_orgao_acesso_externo` = "0",
                    `hash` = "9d04b96c0ecb67ebbf004d9a5b4c0302"
          )
          
          data = list(
                    `txtProtocoloPesquisa` = "",
                    `q` = "",
                    `chkSinProcessos` = "P",
                    `chkSinDocumentosGerados` = "G",
                    `txtParticipante` = "",
                    `hdnIdParticipante` = "",
                    `txtUnidade` = "",
                    `hdnIdUnidade` = "",
                    `selTipoProcedimentoPesquisa` = "",
                    `selSeriePesquisa` = "50001",
                    `txtDataInicio` = "",
                    `txtDataFim` = "",
                    `txtCaptcha` = "",
                    `txtNumeroDocumentoPesquisa` = "",
                    `txtAssinante` = "",
                    `hdnIdAssinante` = "",
                    `txtDescricaoPesquisa` = "",
                    `txtAssunto` = "",
                    `hdnIdAssunto` = "",
                    `txtSiglaUsuario1` = "",
                    `txtSiglaUsuario2` = "",
                    `txtSiglaUsuario3` = "",
                    `txtSiglaUsuario4` = "",
                    `hdnSiglasUsuarios` = "",
                    `hdnSiglasUsuarios` = "",
                    `hdnCaptchaMd5` = "9d04b96c0ecb67ebbf004d9a5b4c0302",
                    `partialfields` = "id_serie:50001 AND sta_prot:P;G",
                    `requiredfields` = "",
                    `as_q` = "",
                    `hdnFlagPesquisa` = "1"
          )
          
          res <- httr::POST(url = "https://sei.tre-rr.jus.br/sei/modulos/pesquisa/md_pesq_processo_pesquisar.php", httr::add_headers(.headers=headers), query = params, httr::set_cookies(.cookies = cookies), body = data, encode = "form")
}

obter.dados <- function(pagina) {
          pa <- pagina |>
                    rvest::html_elements('.pesquisaTituloEsquerda') |>
                    rvest::html_element('.protocoloNormal') |>
                    rvest::html_text2()
          
          endereco_pa <- pagina |>
                    rvest::html_elements('.pesquisaTituloEsquerda') |>
                    rvest::html_element('.protocoloNormal') |>
                    rvest::html_attr('href')
          
          documento <- pagina |>
                    rvest::html_elements('.pesquisaTituloEsquerda') |>
                    rvest::html_element('a:last-of-type') |>
                    rvest::html_text2()
          
          endereco_doc <- pagina |>
                    rvest::html_elements('.pesquisaTituloEsquerda') |>
                    rvest::html_element('a:last-of-type') |>
                    rvest::html_attr('href')
          
          unidade <- pagina |>
                    rvest::html_elements('.ancoraSigla') |>
                    rvest::html_text2()
          
          data <- pagina |>
                    rvest::html_elements('.pesquisaMetatag:nth-child(3)') |>
                    rvest::html_text2() |>
                    as.Date.character(format = 'Inclusão: %d/%m/%Y')
          
          dados <- data.frame(
                    pa = pa,
                    endereco_pa = paste0('https://sei.tre-rr.jus.br/sei/modulos/pesquisa/',
                                         endereco_pa),
                    documento = documento,
                    endereco_doc = paste0('https://sei.tre-rr.jus.br/sei/modulos/pesquisa/',
                                          endereco_doc),
                    unidade = unidade,
                    data = data,
                    texto = NA,
                    autoridade = NA,
                    cargo = NA,
                    ano = NA
          )
          
          dados.novos <- dados[!dados$endereco_doc %in% dados.anteriores$endereco_doc, ]
          
          print(paste0('Foram localizados ', nrow(dados.novos), ' novos documentos nesta consulta.'))
          
          return(dados)
}

obter.dados.tabela <- function(pagina) {
          tabela <- pagina |>
                    rvest::html_elements('table') |>
                    rvest::html_table(header=F, fill=T)
          
          tabela <- tabela[[1]]
          
          # Verifique se a tabela não está vazia
          if (length(tabela) == 0) {
                    stop('Nenhuma tabela encontrada na página.')
          }
          
          dados <- data.frame(
                    pa = character(0),
                    documento = character(0),
                    endereco_doc = character(0),
                    unidade = character(0),
                    data = integer(0),
                    texto = character(0),
                    autoridade = character(0),
                    cargo = character(0),
                    ano = integer(0)
          )
          
          for (i in seq(1, nrow(tabela), by = 3)) {
                    observacao <- sub('.+n°', '', tabela[i,1])
                    pa <- strsplit(observacao, " \\(")[[1]][1]
                    documento <- sub('\\)', '', strsplit(observacao, " \\(")[[1]][2])
                    unidade <- sub('Unidade: ', '', as.character(tabela[i+2,1]))
                    data <- as.Date.character(tabela[i+2,3], format = 'Inclusão: %d/%m/%Y')
                    
                    # Adicione os valores ao dataframe
                    dados <- rbind(dados, data.frame(
                              pa = pa,
                              documento = documento,
                              endereco_doc = NA,
                              unidade = unidade,
                              data = data,
                              texto = NA,
                              autoridade = NA,
                              cargo = NA,
                              ano = NA
                    ))
                    
                    for (documento in dados$documento) {
                              xpath_query <- paste0("//a[contains(text(), '", documento, "')]")  # Constrói a consulta XPath
                              endereco_doc <- pagina |>
                                        rvest::html_elements(xpath = xpath_query) |>
                                        rvest::html_attr('href')
                              dados$endereco_doc[dados$documento == documento] <- paste0('https://sei.tre-rr.jus.br/sei/modulos/pesquisa/',
                                                                                         endereco_doc)
                    }    
                    
          }
          
          if (any(is.na(dados))) {
                    stop('Algumas observações têm variáveis incompletas.')
          }
          
          dados.novos <- dados[!dados$endereco_doc %in% dados.anteriores$endereco_doc, ]
          
          print(paste0('Foram localizados ', nrow(dados.novos), ' novos documentos nesta consulta.'))
          
          return(dados)
}

extrair.texto <- function(item) {
          texto <- dados.novos$endereco_doc[item] |>
                    rvest::read_html() |>
                    rvest::html_text2()
          
          Sys.sleep(.5)
          
          return(texto)
          
}

extrair.ementa <- function(item) {
          #tentando extrair o primeiro artigo da portaria
          texto <- dados.novos$texto[item] |>
                    stringr::str_match('\\nArt\\.*.+') |>
                    stringr::str_remove('\\nArt\\. *\\d+\\.*[°|º]*\\.* *[–|-]* *')
          
          #tentando extrair o primeiro parágrafo após o 'resolve:' da portaria
          if (is.na(texto)) {
                    texto <- dados.novos$texto[item] |>
                              stringr::str_match('R.+:\\W+(\\w+,* .+)')
                    
                    
                    texto <- texto[, 2]
                    
          }
          
          return(texto)
          
}

extrair.autoridade <- function(item) {
          texto <- dados.novos$texto[item] |>
                    stringr::str_match('Documento assinado eletronicamente por (.+?), (.+?),')
          return(texto)
}
