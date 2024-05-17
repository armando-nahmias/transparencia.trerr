
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
          
          if (as.POSIXlt(Sys.Date())$wday == 1) file.remove(dplyr::last(destinos))
          
          curl::multi_download(fontes,
                               destinos,
                               resume = TRUE,
                               progress = TRUE)
}

importar.contrato.comprasnet <- function() {
          
          arquivo <- '../rds/contratos.rds'
          dados <- readr::read_rds(arquivo)
          
          contratos <- dados |> purrr::pluck('contratos')
          atualizado <- dados |> purrr::pluck('atualizado')
          
          if (lubridate::dmy(atualizado) == Sys.Date() && 'consultado' %in% names(dados)) {
                    logger::log_info('Não é necessário baixar novamente, pois a última atualização é de {atualizado}.')
                    return()
          }
          
          if (lubridate::month(lubridate::dmy(atualizado)) != lubridate::month(Sys.Date())) {
                    source('../src/organizar.R')
                    dados <- consolidar.contratos.anual.seges()
                    contratos <- dados |> purrr::pluck('contratos') |> dplyr::select(id, numero)
                    atualizado <- dados |> purrr::pluck('atualizado')
          }

          resultados <- list()
          n.recursos <<- 0
          total.recursos <<- 0
          recurso <- 'contrato'
          
          for (i in seq_along(contratos$id)) {
                    id <- contratos$id[i]
                    numero <- contratos$numero[i]
                    
                    logger::log_info('Iniciando a consulta número {numero} de um total de {total}.',
                                     numero = which(contratos$id == id),
                                     total = nrow(contratos))
                    
                    resultados[[length(resultados) + 1]] <- raspar.comprasnet(id, numero, recurso)
          }
          
          consultado <- data.table::rbindlist(resultados)

          atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
          
          dados$atualizado <- atualizado
          dados$consultado <- consultado
          
          readr::write_rds(dados, arquivo)
          
}

importar.recurso.comprasnet <- function(recurso) {
          # recurso <- 'garantias'

          arquivo <- sprintf('../rds/%s.rds', recurso)
          
          if (file.exists(arquivo)) {
                    dados <- readr::read_rds(arquivo)
                    contratos <- dados |> purrr::pluck('contratos')
                    atualizado <- dados |> purrr::pluck('atualizado')
                    if (nrow(contratos) == 0) {
                              logger::log_info('Não há contratos para consultar.')
                              return()
                    }
                    if (lubridate::dmy(atualizado) == Sys.Date()) {
                              logger::log_info('Não é necessário baixar novamente, pois a última atualização é de {atualizado}.')
                              return()
                    }
          }
          
          if (!exists('atualizado') || lubridate::month(lubridate::dmy(atualizado)) != lubridate::month(Sys.Date())) {
                    if (file.exists('../rds/contratos.rds')) {
                              dados <- readr::read_rds('../rds/contratos.rds')
                    } else {
                              source('../src/organizar.R')
                              dados <- consolidar.contratos.anual.seges()
                    }
                    contratos <- dados |> purrr::pluck('contratos') |> dplyr::select(id, numero)
          }
          
          resultados <- list()
          n.recursos <<- 0
          total.recursos <<- 0

          for (i in seq_along(contratos$id)) {
                    id <- contratos$id[i]
                    numero <- contratos$numero[i]

                    logger::log_info('Iniciando a consulta número {numero} de um total de {total}.',
                                     numero = which(contratos$id == id),
                                     total = nrow(contratos))
                    
                    resultados[[length(resultados) + 1]] <- raspar.comprasnet(id, numero, recurso)
          }
          
          consultado <- data.table::rbindlist(resultados)

          if (length(contratos) != nrow(consultado)) {

                    contratos <- contratos |> 
                              dplyr::filter(id %in% consultado$contrato_id)
          }
          
          atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
          
          dados$atualizado <- atualizado
          dados$contratos <- contratos
          dados$consultado <- consultado
          
          readr::write_rds(dados, arquivo)
          
}

importar.precos.combustiveis <- function() {
          arquivo <- '../rds/combustiveis.rds'

          precos.combustiveis.semanal.fonte <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/semanal/semanal-municipios-2022-2024.xlsx'
          
          precos.combustiveis.mensal.fonte <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/mensal/mensal-municipios-jan2022-2024.xlsx'
          
          fontes <- c(semanal = precos.combustiveis.semanal.fonte, mensal = precos.combustiveis.mensal.fonte)
          
          destinos <- c(semanal = '../dados/anp.precos.medios.semanal.xlsx', mensal = '../dados/anp.precos.medios.mensal.xlsx')
          
          if (as.POSIXlt(Sys.Date())$wday == 1) file.remove(destinos)
          
          curl::multi_download(fontes,
                               destinos,
                               resume = TRUE,
                               progress = TRUE)
          
          semanal <-
                    readxl::read_xlsx(destinos[[1]],
                                      col_types = 'text',
                                      skip = 11)
          
          mensal <-
                    readxl::read_xlsx(destinos[[2]],
                                      col_types = 'text',
                                      skip = 16)
          
          municipios <- list('BOA VISTA')
          produtos <-
                    list('GASOLINA COMUM', 'OLEO DIESEL', 'OLEO DIESEL S10', 'GLP')
          colunas.novas.semanal <-
                    c(
                              'Inicio',
                              'Fim',
                              'Estado',
                              'Municipio',
                              'Produto',
                              'Medida',
                              'Preço médio',
                              'Preço mínimo'
                    )
          colunas.atuais.semanal <-
                    c(
                              "DATA INICIAL",
                              "DATA FINAL",
                              "ESTADO",
                              "MUNICÍPIO",
                              "PRODUTO",
                              "UNIDADE DE MEDIDA",
                              "PREÇO MÉDIO REVENDA",
                              "PREÇO MÍNIMO REVENDA"
                    )
          colunas.novas.mensal <-
                    c(
                              'Inicio',
                              'Produto',
                              'Estado',
                              'Municipio',
                              'Medida',
                              'Preço médio',
                              'Preço mínimo'
                    )
          colunas.atuais.mensal <-
                    c(
                              "MÊS",
                              "PRODUTO",
                              "ESTADO",
                              "MUNICÍPIO",
                              "UNIDADE DE MEDIDA",
                              "PREÇO MÉDIO REVENDA",
                              "PREÇO MÍNIMO REVENDA"
                    )
          
          consultado1 <- semanal |>
                    dplyr::filter(`MUNICÍPIO` %in% municipios,
                                  PRODUTO %in% produtos,
                                  `DATA INICIAL` == max(`DATA INICIAL`)) |>
                    dplyr::select(1, 2, 4, 5, 6, 8, 9, 11) |>
                    dplyr::rename(setNames(colunas.atuais.semanal, colunas.novas.semanal)) |> 
                    dplyr::mutate(Periodicidade = 'Semanal')

          consultado2 <- mensal |>
                    dplyr::filter(`MUNICÍPIO` %in% municipios,
                                  PRODUTO %in% produtos,
                                  `MÊS` == max(`MÊS`)) |> 
                    dplyr::select(1, 2, 4, 5, 7, 8, 10) |>
                    dplyr::rename(setNames(colunas.atuais.mensal, colunas.novas.mensal)) |> 
                    dplyr::mutate(Periodicidade = 'Mensal')
          
          consultado <- dplyr::bind_rows(consultado1, consultado2)
          
          atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
          
          dados <- list(atualizado = atualizado, consultado = consultado)
          
          readr::write_rds(dados, arquivo)
          
}


# Funções secundárias -----------------------------------------------------


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
                    ifelse(recurso == 'contrato', 
                           epoxy::epoxy('https://contratos.comprasnet.gov.br/api/v1/contrato/id/{id}'),
                           epoxy::epoxy('https://contratos.comprasnet.gov.br/api/v1/contrato/{id}/{recurso}'))
          
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

