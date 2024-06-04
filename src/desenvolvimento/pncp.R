

# enderecos ---------------------------------------------------------------

contrato <- 'https://pncp.gov.br/api/consulta/v1/contratos?dataInicial=20230101&dataFinal=20231231&cnpjOrgao=00509018000113&codigoUnidadeAdministrativa=070028&pagina=1'
arp <-   'https://pncp.gov.br/api/consulta/v1/atas?dataInicial=20220101&dataFinal=20221231&cnpj=00509018000113&codigoUnidadeAdministrativa=070028&pagina=1'
publicacao <-   'https://pncp.gov.br/api/consulta/v1/contratacoes/publicacao?dataInicial=20240101&dataFinal=20240520&codigoModalidadeContratacao=6&cnpj=00509018000113&codigoUnidadeAdministrativa=070028&pagina=1'
proposta <-   'https://pncp.gov.br/api/consulta/v1/contratacoes/proposta?dataFinal=20241231&cnpj=00509018000113&codigoUnidadeAdministrativa=070028&pagina=1'





# importar ---------------------------------------------------------------
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

raspar.pncp <- function(recurso, modalidade, data.inicial, data.final, pagina) {
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
                              'Consultando página {pagina} da modalidade {nome} de {recurso} no ano {ano}.
                                     Endereço: {endereco}',
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
          
          dados <- list(atualizado = atualizado, consultado = consultado)
          
          if (!exists('erro')) {
                    readr::write_rds(dados, epoxy::epoxy('rds/pncp.{recurso}.rds'))
          }
          
}



# organizar ---------------------------------------------------------------
