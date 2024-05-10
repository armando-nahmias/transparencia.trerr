raspar.comprasnet <- function(id, numero, recurso) {
          if (!exists('cabecalho')) {
                    acesso.API <- function() {
                              cabecalho <- c(`accept` = "application/json",
                                             `Content-Type` = "application/json")
                              
                              configuracao <- jsonlite::fromJSON('../configuracao/configuracao.json')
                              corpo <-
                                        jsonlite::toJSON(
                                                  list(
                                                            cpf = configuracao$usuario.api,
                                                            password = configuracao$senha.api
                                                  ),
                                                  auto_unbox = TRUE
                                        )
                              
                              resposta <-
                                        httr::POST(
                                                  url = "https://contratos.comprasnet.gov.br/api/v1/auth/login",
                                                  httr::add_headers(.headers = cabecalho),
                                                  body = corpo
                                        )
                              
                              if (httr::http_type(resposta) == "application/json") {
                                        conteudo <- httr::content(resposta)
                                        if (!is.null(conteudo$access_token)) {
                                                  simbolo <- conteudo$access_token
                                                  cat("Senha de acesso gerada.")
                                                  
                                                  return(simbolo)
                                        } else {
                                                  stop(
                                                            "Senha de acesso não encontrado na resposta."
                                                  )
                                        }
                              } else {
                                        stop("Falha na solicitação POST.")
                              }
                    }
                    simbolo <- acesso.API()
                    cabecalho <<- c(
                              `accept` = 'application/json',
                              `Authorization` = paste0('Bearer ', simbolo)
                    )
          }
          
          endereco <-
                    epoxy::epoxy('https://contratos.comprasnet.gov.br/api/v1/contrato/{id}/{recurso}')
          
          resposta <-
                    httr::GET(endereco, httr::add_headers(.headers = cabecalho))
          
          if (resposta$status_code == 200) {
                    conteudo <- httr::content(resposta,
                                              as = 'text',
                                              encoding = 'UTF-8')
                    dados <- jsonlite::fromJSON(conteudo)
                    n.recursos <<- ifelse(is.null(nrow(dados)), 0, nrow(dados))
                    total.recursos <<- total.recursos + n.recursos
                    
                    if (n.recursos == 0) {
                              logger::log_warn(
                                        'A consulta não retornou resultados para o Contrato {id} de número {numero}.
                                        Desde o início da consulta, foram encontrados {total.recursos} {recurso}.'
                              )
                              
                              return(dados)
                    } else {
                              logger::log_info(
                                        'Consulta bem-sucedida para o Contrato {id} de número {numero}.
                                        O contrato consultado possui {n.recursos} {recurso}.
                                        Desde o início da consulta, foram encontrados {total.recursos} {recurso}.'
                              )
                              
                              return(dados)
                    }
          } else {
                    logger::log_error(
                              'Ocorreu um erro na consulta do Contrato {numero}. Código do erro: {resposta$status_code}.'
                    )
                    return(NULL)
          }
}

importar.precos.combustiveis <- function() {
          precos.combustiveis.semanal.fonte <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/semanal/semanal-municipios-2022-2024.xlsx'
          
          precos.combustiveis.mensal.fonte <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/mensal/mensal-municipios-jan2022-2024.xlsx'
          
          fontes <- c(semanal = precos.combustiveis.semanal.fonte, mensal = precos.combustiveis.mensal.fonte)
          
          destinos <- c(semanal = '../dados/anp.precos.medios.semanal.xlsx', mensal = '../dados/anp.precos.medios.mensal.xlsx')
          
          curl::multi_download(fontes,
                               destinos,
                               resume = TRUE,
                               progress = TRUE)
}

importar.contratos.anual.seges <- function() {
          anos <- seq(2021, as.numeric(format(Sys.Date(), "%Y")))
          destinos <- sprintf('../dados/seges.contratos.anual.%s.csv', anos)
          arquivos <-
                    sprintf('comprasnet-contratos-anual-contratos-%s.csv',
                            anos)
          fontes <-
                    sprintf(
                              'https://repositorio.dados.gov.br/seges/comprasnet_contratos/anual/%s/%s',
                              anos,
                              arquivos
                    )
          
          curl::multi_download(fontes,
                               destinos,
                               resume = TRUE,
                               progress = TRUE)
}

consultar.recurso <- function(recurso) {
          arquivo <- sprintf('../rds/%s.rds', recurso)
          
          if (file.exists(arquivo)) {
                    contratos <- readr::read_rds(arquivo) |> purrr::pluck(2)
                    atualizado <- readr::read_rds(arquivo) |> purrr::pluck(1)
          } 
          
          if (nrow(contratos) == 0) {
                    return()
          }
          
          if (!exists('contratos') || lubridate::month(lubridate::dmy(atualizado)) != lubridate::month(Sys.Date())) {
                    source('../src/transformar.R')
                    contratos <- gerar.lista.contratos()
          }

          resultados <- list()
          n.recursos <<- 0
          total.recursos <<- 0
          n.contratos <- length(contratos)
          
          for (i in seq_along(contratos$id)) {
                    id <- contratos$id[i]
                    numero <- contratos$numero[i]
                    resultados[[length(resultados) + 1]] <- raspar.comprasnet(id, numero, recurso)
          }
          
          consulta <- do.call(rbind, resultados)
          
          if (length(contratos) != nrow(consulta)) {

                    contratos <- contratos |> 
                              dplyr::filter(id %in% consulta$contrato_id)
          }
          
          atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
          
          dados <- list(atualizado, contratos, consulta)
          
          readr::write_rds(dados, arquivo)
          
}
