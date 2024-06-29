
# seges -------------------------------------------------------------------

importar.contratos.anual.seges <- function() {
          anos <- seq(2021, as.numeric(format(Sys.Date(), "%Y")))
          destinos <- sprintf('dados/seges.contratos.anual.%s.csv', anos)
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

# comprasnet --------------------------------------------------------------

importar.contrato.comprasnet <- function() {
          
          arquivo <- 'rds/contratos.rds'
          dados <- readr::read_rds(arquivo)
          
          contratos <- dados |> purrr::pluck('contratos')
          atualizado <- dados |> purrr::pluck('atualizado')
          
          if (lubridate::dmy(atualizado) == Sys.Date() && 'consultado' %in% names(dados)) {
                    logger::log_info('Não é necessário baixar novamente, pois a última atualização é de hoje.')
                    return()
          }
          
          if (lubridate::month(lubridate::dmy(atualizado)) != lubridate::month(Sys.Date())) {
                    source('src/organizar.R')
                    dados <- consolidar.contratos.anual.seges()
                    contratos <- dados |> purrr::pluck('contratos')
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
          dados$comunicado <- ''
          
          readr::write_rds(dados, arquivo)
          
}

importar.recurso.comprasnet <- function(recurso) {
          # recurso <- 'garantias'

          arquivo <- sprintf('rds/%s.rds', recurso)
          
          if (file.exists(arquivo)) {
                    dados <- readr::read_rds(arquivo)
                    contratos <- dados |> purrr::pluck('contratos')
                    atualizado <- dados |> purrr::pluck('atualizado')
                    if (nrow(contratos) == 0) {
                              logger::log_info('Não há contratos para consultar.')
                              return()
                    }
                    if (lubridate::dmy(atualizado) == Sys.Date()) {
                              logger::log_info('Não é necessário baixar novamente, pois a última atualização é de hoje.')
                              return()
                    }
          }
          
          if (!exists('atualizado') || lubridate::month(lubridate::dmy(atualizado)) != lubridate::month(Sys.Date())) {
                    if (file.exists('rds/contratos.rds')) {
                              dados <- readr::read_rds('rds/contratos.rds')
                    } else {
                              source('src/organizar.R')
                              dados <- consolidar.contratos.anual.seges()
                    }
                    contratos <- dados |> purrr::pluck('contratos')
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
          dados$comunicado <- ''
          
          readr::write_rds(dados, arquivo)
          
}


# pncp --------------------------------------------------------------------

importar.pncp.recursos <- function() {
          cnpj.orgao <<- '00509018000113'
          codigo.unidade <<- '070028'
          recursos <- c('contratos', 'atas')
          modalidade <- list("Inexistente" = 99)
          
          for (recurso in recursos) {
                    consultado <- tibble::tibble()
                    pagina <- 1
                    data.inicial <- '20230101'
                    data.final <- stringr::str_remove_all(Sys.Date(), '-')
                    sequencia.datas <- gerar.sequencia.datas(data.inicial, data.final)
                    
                    for (i in seq_len(nrow(sequencia.datas))) {
                              data.inicio <- sequencia.datas$datas.inicio[i]
                              data.fim <- sequencia.datas$datas.fim[i]
                              
                              consulta <<- TRUE
                              pagina <- 1
                              
                              while (consulta) {
                                        resultado <- raspar.pncp(
                                                  recurso,
                                                  modalidade,
                                                  data.inicio,
                                                  data.fim,
                                                  pagina
                                        )
                                        if (!is.null(resultado)) {
                                                  consultado <- dplyr::bind_rows(consultado,
                                                                                 resultado)
                                        }
                                        pagina <- pagina + 1
                              }
                              
                    }
                    
                    atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
                    
                    dados <- list(atualizado = atualizado,
                                  consultado = consultado,
                                  comunicado = '')
                    
                    if (!exists('erro')) {
                              readr::write_rds(dados,
                                               epoxy::epoxy('rds/pncp.{recurso}.rds'))
                    } else {
                              rm('erro')
                    }
                    
          }
}

importar.pncp.publicacao <- function() {
          cnpj.orgao <<- '00509018000113'
          codigo.unidade <<- '070028'
          recurso <<- c('publicacao')
          modalidades <<- list(
                    # "Leilao" = 1,
                    # "Dialogo" = 2,
                    # "Concurso" = 3,
                    # "ConcorrenciaEletronica" = 4,
                    # "ConcorrenciaPresencial" = 5,
                    # "PregaoPresencial" = 7,
                    # "Interesse" = 10,
                    # "Prequalificacao" = 11,
                    # "Credenciamento" = 12,
                    # "LeilaoPresencial" = 13,
                    "Dispensabilidade" = 8,
                    "Inexigibilidade" = 9,
                    "Pregao" = 6
          )
          
          consultado <- tibble::tibble()
          pagina <- 1
          data.inicial <- '20230101'
          data.final <- stringr::str_remove_all(Sys.Date(), '-')
          sequencia.datas <- gerar.sequencia.datas(data.inicial, data.final)
          
          
          for (modalidade in modalidades) {
                    
                    for (i in seq_len(nrow(sequencia.datas))) {
                              data.inicio <- sequencia.datas$datas.inicio[i]
                              data.fim <- sequencia.datas$datas.fim[i]
                              
                              consulta <<- TRUE
                              pagina <- 1
                              
                              while (consulta) {
                                        resultado <- raspar.pncp(
                                                  recurso,
                                                  modalidade,
                                                  data.inicio,
                                                  data.fim,
                                                  pagina
                                        )
                                        if (!is.null(resultado)) {
                                                  consultado <- dplyr::bind_rows(consultado,
                                                                                 resultado)
                                        }
                                        pagina <- pagina + 1
                              }
                    }
          }
          
          atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
          
          dados <- list(atualizado = atualizado, consultado = consultado,
                        comunicado = '')
          
          if (!exists('erro')) {
                    readr::write_rds(dados, epoxy::epoxy('rds/pncp.{recurso}.rds'))
          } else {
                    rm('erro')
          }
          
}


# anp ---------------------------------------------------------------------

importar.precos.combustiveis <- function() {
          arquivo <- 'rds/combustiveis.rds'
          
          precos.combustiveis.semanal.fonte <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/semanal/semanal-municipios-2022-2024.xlsx'
          
          precos.combustiveis.mensal.fonte <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/mensal/mensal-municipios-jan2022-2024.xlsx'
          
          fontes <- c(semanal = precos.combustiveis.semanal.fonte, mensal = precos.combustiveis.mensal.fonte)
          
          destinos <- c(semanal = 'dados/anp.precos.medios.semanal.xlsx', mensal = 'dados/anp.precos.medios.mensal.xlsx')
          
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
          
          dados <- list(atualizado = atualizado, consultado = consultado, comunicado = '')
          
          readr::write_rds(dados, arquivo)
          
}



# orcamento ---------------------------------------------------------------

importar.execucao.detalhada <- function() {
          arquivo <- 'rds/execucao.detalhada.rds'
          
          origem.dados <- 'dados/execucao.geral.txt'
          colunas <- c("AG", "Ação Governo", "PO", "Plano Orçamentário", "GD", "Grupo Despesa", "ND", "Natureza Despesa Detalhada", "Nota de Empenho", "PI", "Plano Interno", "Favorecido", "Processo", "Despesas Empenhadas", "Despesas a Liquidar", "Despesas Pagas")
          
          consultado <- readr::read_csv2(origem.dados, skip = 7, col_names = colunas, col_types = readr::cols(.default = 'c'), locale = readr::locale(decimal_mark = ',', grouping_mark = '.'))
          
          atualizado <- format(as.Date(file.info(origem.dados)$mtime) - 1, format = '%d/%m/%Y')
          
          dados <- list(atualizado = atualizado, consultado = consultado, comunicado = '')
          
          readr::write_rds(dados, arquivo)
}

# Funções secundárias -----------------------------------------------------

gerar.sequencia.datas <- function(data.inicial, data.final) {
          # data.inicial <- '20220501'
          # data.final <- '20240520'
          
          ano.inicial <- lubridate::year(lubridate::ymd(data.inicial))
          ano.final <- lubridate::year(lubridate::ymd(data.final))
          
          anos <- ano.inicial:ano.final
          
          datas.inicio <- paste0(anos, "0101")
          datas.fim <- paste0(anos, "1231")
          
          sequencia.datas <- tibble::tibble(datas.inicio = datas.inicio, datas.fim = datas.fim)
          
          if (data.final != datas.fim[length(datas.fim)]) {
                    sequencia.datas$datas.fim[nrow(sequencia.datas)] <- data.final
          }
          if (data.inicial != datas.inicio[1]) {
                    sequencia.datas$datas.inicio[1] <- data.inicial
          }
          
          return(sequencia.datas)
}

raspar.comprasnet <- function(id, numero, recurso) {
          if (!exists('cabecalho')) {
                    acesso.API <- function() {
                              cabecalho <- c(`accept` = "application/json",
                                             `Content-Type` = "application/json")
                              
                              configuracao <- jsonlite::fromJSON('configuracao/configuracao.json')
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

raspar.pncp <- function(recurso,
                        modalidade,
                        data.inicial,
                        data.final,
                        pagina) {
          if (recurso == 'proposta') {
                    if (data.final == stringr::str_remove_all(Sys.Date(), '-')) {
                              ano <- lubridate::year(lubridate::ymd(data.inicial))
                              data.final <- paste0(ano, "1231")
                    } else {
                              logger::log_warn(
                                        'Não há dados para a consulta da página {pagina} de {recurso} no ano {ano}.',
                                        ano = lubridate::year(lubridate::ymd(
                                                  data.inicial
                                        ))
                              )
                              consulta <<- FALSE
                              erro <<- TRUE
                              return(NULL)
                    }
          }
          
          endereco <- dplyr::case_when(
                    recurso == 'contratos' ~ epoxy::epoxy(
                              'https://pncp.gov.br/api/consulta/v1/{recurso}?dataInicial={data.inicial}&dataFinal={data.final}&cnpjOrgao={cnpj.orgao}&codigoUnidadeAdministrativa={codigo.unidade}&pagina={pagina}'
                    ),
                    recurso == 'atas' ~ epoxy::epoxy(
                              'https://pncp.gov.br/api/consulta/v1/{recurso}?dataInicial={data.inicial}&dataFinal={data.final}&cnpj={cnpj.orgao}&codigoUnidadeAdministrativa={codigo.unidade}&pagina={pagina}'
                    ),
                    recurso == 'publicacao' ~ epoxy::epoxy(
                              'https://pncp.gov.br/api/consulta/v1/contratacoes/{recurso}?dataInicial={data.inicial}&dataFinal={data.final}&codigoModalidadeContratacao={modalidade}&&cnpj={cnpj.orgao}&codigoUnidadeAdministrativa={codigo.unidade}&pagina={pagina}'
                    ),
                    recurso == 'proposta' ~ epoxy::epoxy(
                              'https://pncp.gov.br/api/consulta/v1/contratacoes/proposta?dataFinal={data.final}&cnpj={cnpj.orgao}&codigoUnidadeAdministrativa={codigo.unidade}&pagina={pagina}'
                    ),
                    TRUE ~ NA_character_
          )
          
          if (modalidade == 99) {
                    logger::log_warn(
                              'Consultando página {pagina} de {recurso} no ano {ano}.
                                     Endereço: {endereco}',
                              ano = lubridate::year(lubridate::ymd(data.inicial))
                    )
          } else {
                    logger::log_warn(
                              'Consultando página {pagina} da modalidade {nome} de {recurso} no ano {ano}.',
                              ano = lubridate::year(lubridate::ymd(data.inicial)),
                              nome = names(modalidades[modalidades == modalidade])
                    )
                    
          }
          
          resposta <- httr::GET(endereco)
          
          if (resposta$status_code == 200) {
                    conteudo <- httr::content(resposta,
                                              as = 'text',
                                              encoding = 'UTF-8')
                    dados <- jsonlite::fromJSON(conteudo)
                    
                    if (dados$empty == TRUE) {
                              logger::log_warn('A consulta não retornou resultados.')
                              consulta <<- FALSE
                              return(NULL)
                    } else {
                              logger::log_info('Consulta bem-sucedida.')
                              if (dados$paginasRestantes == 0)
                                        consulta <<- FALSE
                              return(tibble::tibble(dados$data))
                    }
          } else {
                    logger::log_error(
                              'Ocorreu um erro na consulta dos Contratos. Código do erro: {resposta$status_code}.'
                    )
                    consulta <<- FALSE
                    erro <<- TRUE
                    return(NULL)
          }
}




