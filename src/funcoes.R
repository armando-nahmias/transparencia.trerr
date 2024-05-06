

# Enviar mensagem ---------------------------------------------------------

enviar.mensagem <- function(assunto, corpo, anexos, destinatarios) {
          # assunto <- 'Novo Teste'
          # corpo <- 'Esta é uma mensagem de teste.'
          # anexos <- file.path('src', list.files('src', 'R'))
          # DESTINATARIOS <- 'armando.nahmias@tjrr.jus.br'
          # mensagem <- mensagem |> emayili::html(htmltools::h2('Relatorio'),htmltools::p(''))
          
          
          configuracao <-
                    jsonlite::fromJSON('configuracao/configuracao.json')
          
          mensagem <- emayili::envelope(
                    to = destinatarios,
                    from = configuracao$usuario.smtp,
                    subject = assunto,
                    text = corpo
          )
          
          if (exists('anexos') && length(anexos) > 0) {
                    for (anexo in anexos) {
                              mensagem <- mensagem |> emayili::attachment(anexo)
                    }
                    
          } else {
                    anexos <- NULL
          }
          
          configuracao.servidor <- emayili::server(
                    host = 'smtp.gmail.com',
                    port = 465,
                    username = configuracao$usuario.smtp,
                    password = configuracao$senha.smtp
          )
          
          configuracao.servidor(mensagem)
          
          if (exists('mensagem')) {
                    logger::log_info('Mensagem enviada com sucesso!')
          } else {
                    logger::log_error('Falha ao enviar a mensagem.')
                    
          }
}


# Acesso API --------------------------------------------------------------

acesso.API <- function() {
          cabecalho <- c(`accept` = 'application/json',
                         `Content-Type` = 'application/json')
          
          configuracao <-
                    jsonlite::fromJSON('../configuracao/configuracao.json')
          corpo <-
                    jsonlite::toJSON(
                              list(
                                        cpf = configuracao$usuario.api,
                                        password = configuracao$senha.api
                              ),
                              auto_unbox = TRUE
                    )
          
          resposta <-
                    httr::POST(url = 'https://contratos.comprasnet.gov.br/api/v1/auth/login',
                               httr::add_headers(.headers = cabecalho),
                               body = corpo)
          
          if (httr::http_type(resposta) == 'application/json') {
                    conteudo <- httr::content(resposta)
                    if (!is.null(conteudo$access_token)) {
                              simbolo <- conteudo$access_token
                              epoxy::epoxy('Senha de acesso gerada: {simbolo}\n')
                              return(simbolo)
                    } else {
                              stop('Senha de acesso não encontrado na resposta.')
                    }
          } else {
                    stop('Falha na solicitação POST.')
          }
}

cabecalho.acesso.API <- function() {
          cabecalho <- c(`accept` = 'application/json',
                         `Content-Type` = 'application/json')
          
          configuracao <-
                    jsonlite::fromJSON('../configuracao/configuracao.json')
          corpo <-
                    jsonlite::toJSON(
                              list(
                                        cpf = configuracao$usuario.api,
                                        password = configuracao$senha.api
                              ),
                              auto_unbox = TRUE
                    )
          
          resposta <-
                    httr::POST(url = 'https://contratos.comprasnet.gov.br/api/v1/auth/login',
                               httr::add_headers(.headers = cabecalho),
                               body = corpo)
          
          if (httr::http_type(resposta) == 'application/json') {
                    conteudo <- httr::content(resposta)
                    if (!is.null(conteudo$access_token)) {
                              simbolo <- conteudo$access_token
                              epoxy::epoxy('Senha de acesso gerada: {simbolo}\n')
                              cabecalho <- c(`accept` = 'application/json',
                                             `Authorization` = paste0('Bearer ', simbolo))
                              
                              return(cabecalho)
                    } else {
                              stop('Senha de acesso não encontrado na resposta.')
                    }
          } else {
                    stop('Falha na solicitação POST.')
          }
}


# Terceirizados  -----------------------------------------

contratos.terceirizados.conferir <- function() {NULL}

terceirizados.consultar <- function(id, contrato, cabecalho) {
                    endereco <-
                              epoxy::epoxy('https://contratos.comprasnet.gov.br/api/v1/contrato/{id}/terceirizados')
                    
                    resposta <-
                              httr::GET(endereco, httr::add_headers(.headers = cabecalho))
                    
                    if (resposta$status_code == 200) {
                              conteudo <- httr::content(resposta, as = 'text', encoding = 'UTF-8')
                              dados <- jsonlite::fromJSON(conteudo)
                              n.terceirizados <<- ifelse(is.null(nrow(dados)), 0, nrow(dados))
                              total.terceirizados <<- total.terceirizados + n.terceirizados
                              
                              if (n.terceirizados == 0) {
                                        logger::log_warn('A consulta não retornou resultados para o Contrato {contrato}.
                         Desde o início da consulta, foram encontrados {total.terceirizados} terceirizados.')
                                        
                                        return(dados)
                              } else {
                                        logger::log_info(
                                                  'Consulta bem-sucedida para o Contrato {id} de número {contrato}. Foram encontrados {n.terceirizados} terceirizados.
          Desde o início da consulta, foram encontrados {total.terceirizados} terceirizados.'
                                        )
                                        
                                        return(dados)
                              }
                    } else {
                              logger::log_error(
                                        'Ocorreu um erro na consulta do Contrato {contrato}. Código do erro: {resposta$status_code}.'
                              )
                              return(NULL)
                    }
          }

terceirizados.lista.baixar <- function() {
          logger::log_warn(
                    'Início da execução da consulta ao comprasnet para baixar as informações sobre os terceirizados.'
          )
          
          # Configuração
          config <-
                    jsonlite::fromJSON('../configuracao/configuracao.json')
          simbolo <- acesso.API()
          cabecalho <- c(`accept` = 'application/json',
                         `Authorization` = paste0('Bearer ', simbolo))
          
          # Constantes
          codigo.unidade <- list(TRERR = '070028')

          contratos.terceirizados.nomeados <-
                    readRDS('../rds/contratos.terceirizados.nomeados.rds')

          # Instruções
          
          resultados <- list()
          total.terceirizados <<- 0
          n.terceirizados <<- 0
          
          lista.contratos <-
                    readRDS('../rds/contratos.lista.dados.rds') |> 
                    purrr::pluck(2) |> 
                              dplyr::filter(unidade_codigo == codigo.unidade, situacao == 'Ativo')


          for (i in seq_along(contratos.terceirizados.nomeados)) {
                    Sys.sleep(.5)
                    
                    contrato <-
                              names(contratos.terceirizados.nomeados[i])
                    id <-
                              contratos.terceirizados.nomeados[[i]]
                    
                    terceirizados.consultado <-
                              terceirizados.consultar(id, contrato, cabecalho)
                    resultados[[length(resultados) + 1]] <-
                              terceirizados.consultado
          }
          
          if (length(resultados) > 0) {
                    df.terceirizados.consultado <- do.call(rbind, resultados)
                    
          } else {
                    cat('Nenhum terceirizado encontrado.\n')
          }
          
          df.contratos.resumido <-
                    lista.contratos[lista.contratos$id %in% df.terceirizados.consultado$contrato_id,]
          df.unificado <-
                    merge(
                              df.terceirizados.consultado,
                              df.contratos.resumido,
                              by.x = 'contrato_id',
                              by.y = 'id',
                              all.x = TRUE,
                              suffixes = c('.terceirizado', '.contrato')
                    )
          
          df.unificado.ajustado <- df.unificado |>
                    dplyr::mutate(
                              nome = sapply(strsplit(df.unificado$usuario, ' - '), '[[', 2),
                              funcao = ifelse(
                                        is.na(descricao_complementar),
                                        funcao_id,
                                        paste(funcao_id, descricao_complementar)
                              ),
                              funcao = toupper(funcao)
                    ) |>
                    dplyr::arrange(fornecedor_nome, nome) |>
                    dplyr::filter(situacao.terceirizado == 'Ativo')
          
          
          terceirizados.lista <- data.frame(
                    Contrato = df.unificado.ajustado$numero,
                    Fornecedor = df.unificado.ajustado$fornecedor_nome,
                    Terceirizado = df.unificado.ajustado$nome,
                    Função = df.unificado.ajustado$funcao,
                    Lotação = df.unificado.ajustado$unidade,
                    Jornada = df.unificado.ajustado$jornada
          )
          
          atualizacao <- atualizar.monitoramento(item = 65.3)

          terceirizados.lista.dados <-
                    list(atualizacao, terceirizados.lista)
          
          
          saveRDS(terceirizados.lista.dados,
                  '../rds/terceirizados.lista.dados.rds')

          return(terceirizados.lista)
          
          logger::log_warn(
                    'Término da execução da consulta ao comprasnet para baixar as informações sobre os terceirizados.'
          )
          
}

terceirizados.tabela.gerar <- function() {
          terceirizados.lista <-
                    readr::read_rds('../rds/terceirizados.lista.dados.rds') |> 
                    purrr::pluck(2)
          
          atualizacao <-
                    readr::read_rds('../rds/terceirizados.lista.dados.rds') |> 
                    purrr::pluck(1)
          
          
          ## conferir se os dados vêm ordenados e com o ano antes de excluir este comando
          terceirizados.lista <- terceirizados.lista |>
                    dplyr::mutate(Ano = stringr::str_match(Contrato, pattern = "\\/(\\d{4})")[, 2]) |>
                    dplyr::arrange(Fornecedor, Ano, Contrato)
          
          fornecedores <- unique(terceirizados.lista$Fornecedor)
          inicio.repeticao <-
                    which(!duplicated(terceirizados.lista[c('Fornecedor', 'Contrato')]))
          fim.repeticao <-
                    c(inicio.repeticao[-1] - 1, nrow(terceirizados.lista))
          rotulos <-
                    paste("Contrato:",
                          terceirizados.lista$Contrato[inicio.repeticao],
                          '- Fornecedor:',
                          terceirizados.lista$Fornecedor[inicio.repeticao])
          
          terceirizados.tabela <- terceirizados.lista |>
                    dplyr::select(-Fornecedor,-Contrato,-Ano) |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizacao)) |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
          for (i in seq_along(inicio.repeticao)) {
                    terceirizados.tabela <-
                              kableExtra::pack_rows(
                                        terceirizados.tabela,
                                        group_label = rotulos[i],
                                        start_row = inicio.repeticao[i],
                                        end_row = fim.repeticao[i]
                              )
          }
          
          
          readr::write_csv2(terceirizados.lista, '../saida/terceirizados.csv')
          
          kableExtra::save_kable(terceirizados.tabela, '../saida/terceirizados.pdf')
          
          kableExtra::save_kable(terceirizados.tabela, '../saida/terceirizados.html')
          
          return(terceirizados.tabela)
}


# ANP Preços médios -------------------------------------------------------

consultar.precos.combustivel.semanal <- function() {
          endereco <-
                    'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/levantamento-de-precos-de-combustiveis-ultimas-semanas-pesquisadas'
          
          pagina <- rvest::read_html(endereco)
          
          referencia <-
                    'Preços médios semanais: Brasil, regiões, estados e municípios'
          
          fonte.dados <- pagina |>
                    rvest::html_node(xpath = epoxy::epoxy('//a[contains(text(), "{referencia}")]')) |>
                    rvest::html_attr('href')
          
          arquivo.dados.semanal <-
                    '../dados/precos.medios.anp.semanal.xlsx'
          
          curl::multi_download(url = fonte.dados, destfile = arquivo.dados.semanal)
          
          municipios <- list('BOA VISTA')
          produtos.semanal <- list('GLP')
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
          
          precos.medios.semanal <- readxl::read_xlsx(
                    arquivo.dados.semanal,
                    sheet = 'CAPITAIS',
                    col_types = 'text',
                    skip = 9
          )
          
          
          precos.medios.semanal.formatado <-
                    precos.medios.semanal |>
                    dplyr::filter(MUNICÍPIO %in% municipios,
                                  PRODUTO == produtos.semanal) |>
                    dplyr::select(-6, -9, -11, -12) |>
                    dplyr::rename(setNames(colunas.atuais.semanal, colunas.novas.semanal)) |>
                    dplyr::mutate(
                              across(
                                        c(Inicio, Fim),
                                        ~ format(as.Date(as.numeric(.), origin = '1899-12-30'), format = "%d/%m/%Y")
                              ),
                              across(
                                        c(`Preço médio`, `Preço mínimo`),
                                        ~ sprintf("R$ %.2f", round(as.numeric(.), 2))
                              ),
                              across(
                                        c(`Preço médio`, `Preço mínimo`),
                                        ~ stringr::str_replace_all(., "\\.", ",")
                              )
                    )
          
          atualizacao <- atualizar.monitoramento(item = 47.1)
          
          precos.medios.semanal.dados <-
                    list(atualizacao, precos.medios.semanal.formatado)
          
          readr::write_rds(precos.medios.semanal.dados,
                           '../rds/precos.medios.semanal.dados.rds')
          
          readr::write_csv2(precos.medios.semanal.formatado,
                            '../saida/precos.medios.semanal.csv')
          
          precos.medios.semanal.tabela <-
                    kableExtra::kable(
                              precos.medios.semanal.formatado,
                              format = 'html',
                              align = 'llllllrr',
                              caption = epoxy::epoxy('Atualizado em {atualizacao}')
                    ) |>
                    kableExtra::kable_styling(full_width = TRUE,
                                              bootstrap_options = 'striped')
          
          kableExtra::save_kable(precos.medios.semanal.tabela, '../saida/precos.medios.semanal.pdf')
          
          kableExtra::save_kable(precos.medios.semanal.tabela, '../saida/precos.medios.semanal.html')
          
          
}

consultar.precos.combustivel.mensal <- function() {
          fonte.dados <-
                    'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/precos-revenda-e-de-distribuicao-combustiveis/shlp/mensal/mensal_municipios-desde_jan2022_a_2024.xlsx'
          
          arquivo.dados.mensal <-
                    '../dados/precos.medios.anp.mensal.xlsx'
          
          curl::multi_download(url = fonte.dados, destfile = arquivo.dados.mensal)
          
          municipios <- list('BOA VISTA')
          produtos.mensal <-
                    list('GASOLINA COMUM', 'OLEO DIESEL', 'OLEO DIESEL S10')
          colunas.novas.mensal <-
                    c(
                              'Periodo',
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
          
          precos.medios.mensal <-
                    readxl::read_xlsx(arquivo.dados.mensal,
                                      col_types = 'text',
                                      skip = 16)
          
          
          precos.medios.mensal.formatado <- precos.medios.mensal |>
                    dplyr::filter(MUNICÍPIO %in% municipios,
                                  PRODUTO %in% produtos.mensal) |>
                    dplyr::select(1, 2, 4, 5, 7, 8, 10) |>
                    dplyr::rename(setNames(colunas.atuais.mensal, colunas.novas.mensal)) |>
                    dplyr::filter(Periodo == max(Periodo)) |>
                    dplyr::mutate(
                              Periodo = format(as.Date(as.numeric(Periodo), origin = '1899-12-30'), format = "%m/%Y"),
                              across(
                                        c(`Preço médio`, `Preço mínimo`),
                                        ~ sprintf("R$ %.2f", round(as.numeric(.), 2))
                              ),
                              across(
                                        c(`Preço médio`, `Preço mínimo`),
                                        ~ stringr::str_replace_all(., "\\.", ",")
                              )
                    )
          
          atualizacao <- atualizar.monitoramento(item = 47.1)
          
          precos.medios.mensal.dados <-
                    list(atualizacao, precos.medios.mensal.formatado)
          
          readr::write_rds(precos.medios.mensal.dados,
                           '../rds/precos.medios.mensal.dados.rds')
          
          readr::write_csv2(precos.medios.mensal.formatado,
                            '../saida/precos.medios.mensal.csv')
          
          precos.medios.mensal.tabela <-
                    kableExtra::kable(
                              precos.medios.mensal.formatado,
                              format = 'html',
                              align = 'llllllrr',
                              caption = epoxy::epoxy('Atualizado em {atualizacao}')
                    ) |>
                    kableExtra::kable_styling(full_width = TRUE,
                                              bootstrap_options = 'striped')
          
          kableExtra::save_kable(precos.medios.mensal.tabela, '../saida/precos.medios.mensal.pdf')
          
          kableExtra::save_kable(precos.medios.mensal.tabela, '../saida/precos.medios.mensal.html')
          
          
}


# Baixar arquivo ----------------------------------------------------------

baixar.arquivo <- function(arquivo, endereco, destino) {
          tryCatch({
                    logger::log_info('Iniciando a descarga do arquivo {arquivo} para {destino}.')
                    resposta <- httr::GET(endereco)
                    if (httr::status_code(resposta) == 200) {
                              arquivo.baixado <- httr::content(resposta, 'raw')
                              tamanho.arquivo.baixado <-
                                        length(arquivo.baixado)
                              tamanho.arquivo.existente <-
                                        ifelse(file.exists(destino),
                                               file.info(destino)$size,
                                               0)
                              
                              if (tamanho.arquivo.baixado == tamanho.arquivo.existente) {
                                        logger::log_warn('Arquivo baixado igual ao anterior!')
                                        return(TRUE)
                              } else {
                                        writeBin(arquivo.baixado,
                                                 destino)
                                        logger::log_info('Arquivo {arquivo} baixado com sucesso!')
                                        return(TRUE)
                              }
                    }
          }, error = function(e) {
                    logger::log_error('Falha ao baixar o arquivo {arquivo}. Código: {e$message}')
                    falha.descarga <<- TRUE
                    return(FALSE)
          })
}


# Atualizar relação de contratos ------------------------------------------

baixar.contratos.lista <- function() {
          # Log de início do processo de consulta de contratos
          logger::log_warn('Início da consulta à SEGES para baixar os arquivos anuais de contratos.')
          
          # Definindo o intervalo de anos para os contratos, de 2021 até o ano atual
          anos <- seq(2021, as.numeric(format(Sys.Date(), "%Y")))
          
          # Gera os nomes e endereços dos arquivos a serem baixados e seus destinos
          destinos <-
                    sprintf('../dados/contratos.%s.csv', anos)
          arquivos <-
                    sprintf('comprasnet-contratos-anual-contratos-%s.csv',
                            anos)
          enderecos <-
                    sprintf(
                              'https://repositorio.dados.gov.br/seges/comprasnet_contratos/anual/%s/%s',
                              anos,
                              arquivos
                    )
          
          # Verifica se é necessário baixar todos os arquivos
          if (!file.exists('../rds/contratos.lista.dados.rds')) {
                    # Primeira execução: baixa todos os arquivos
                    for (i in seq_along(arquivos)) {
                              logger::log_info('Baixando arquivo ', arquivos[i])
                              baixar.arquivo(arquivos[i],
                                             enderecos[i],
                                             destinos[i])
                    }
          }  else {
                    # Execuções subsequentes: atualiza apenas o arquivo mais recente
                    logger::log_warn('Atualizando apenas o último arquivo devido à existência de cache.')
                    logger::log_info('Baixando arquivo ', arquivos[length(arquivos)])
                    baixar.arquivo(arquivos[length(arquivos)], enderecos[length(enderecos)], destinos[length(destinos)])
          }
          
          if (exists('falha.descarga')) {
                    logger::log_error('Não foi possível fazer a atualização.')
                    return()
          }
          
          # Processamento dos dados baixados para criar um dataframe consolidado
          df1 <-
                    setNames(lapply(destinos, function(arquivo)
                              read.csv(arquivo, colClasses = 'character')), arquivos)
          df2 <-
                    dplyr::bind_rows(Map(function(df1, ano)
                              dplyr::mutate(df1, ano = ano), df1, anos))
          
          # Filtragem e preparação dos dados específicos da Justiça Eleitoral
          contratos.lista <- tibble::as_tibble(df2) |>
                    dplyr::filter(unidade_codigo %in% c('070028', '70028')) |>
                    dplyr::filter(tipo %in% c('Contrato', 'Carta Contrato', 'Empenho')) |>
                    dplyr::filter(situacao == 'Ativo') |>
                    dplyr::arrange(desc(id), desc(ano)) |>
                    dplyr::distinct(id, .keep_all = TRUE)
          
          atualizacao <- atualizar.monitoramento(item = 47)
          
          contratos.lista.dados <-
                    list(atualizacao, contratos.lista)
          
          # Salvando o dataframe processado para uso futuro
          readr::write_rds(contratos.lista.dados,
                           '../rds/contratos.lista.dados.rds')
          
          # Log de término do processo
          logger::log_warn('Término da consulta à SEGES para baixar os arquivos anuais de contratos.')
          
          return(contratos.lista)
}

gerar.contratos.tabela <- function() {
          
          colunas.atuais <-
                    c('numero', 'fornecedor_nome', 'tipo', 'categoria', 'objeto')
          colunas.novas <-
                    c('Contrato', 'Fornecedor', 'Tipo', 'Categoria', 'Objeto')
          
          contratos.lista.dados <- readr::read_rds('../rds/contratos.lista.dados.rds')
          
          contratos.lista.atualizacao <- purrr::pluck(contratos.lista.dados, 1)
          
          contratos.lista <- purrr::pluck(contratos.lista.dados, 2) |> 
                    dplyr::select(numero, fornecedor_nome, tipo, categoria, objeto) |>
                    dplyr::rename(setNames(colunas.atuais, colunas.novas)) |>
                    dplyr::mutate(Ano = dplyr::case_when(
                              Tipo == "Empenho" ~ stringr::str_match(Contrato, pattern = '(\\d{4})NE')[, 1],
                              TRUE ~ stringr::str_match(Contrato, pattern = '\\/(\\d{4})')[, 2]
                    )) |>
                    dplyr::arrange(Tipo, Ano, Contrato) |> 
                    dplyr::select(-Ano)
          
          readr::write_csv2(contratos.lista,
                            '../saida/contratos.csv')
          
          contratos.tabela <- contratos.lista |>
                    kableExtra::kable(format = 'html',
                                      caption = epoxy::epoxy('Atualizado em {contratos.lista.atualizacao}')) |>
                    kableExtra::kable_styling(full_width = TRUE,
                                              bootstrap_options = 'striped')
          
          kableExtra::save_kable(contratos.tabela,
                                 '../saida/contratos.pdf')
          
          kableExtra::save_kable(contratos.tabela,
                                 '../saida/contratos.html')
          
          return(contratos.tabela)
}

# Atualizar monitoramento -------------------------------------------------

atualizar.monitoramento <- function(item) {
          monitoramento <-
                    readr::read_rds('../rds/monitoramento.rds')
          
          atualizacao <- Sys.Date()
          
          monitoramento$Atualizado[monitoramento$Item == item] <- atualizacao
          
          readr::write_rds(monitoramento, '../rds/monitoramento.rds')
          
          return(atualizacao)
}



# Garantias --------------------------------------------------------------

consultar.garantias <- function(id, contrato, cabecalho) {
          endereco <-
                    epoxy::epoxy('https://contratos.comprasnet.gov.br/api/v1/contrato/{id}/garantias')
          
          resposta <-
                    httr::GET(endereco, httr::add_headers(.headers = cabecalho))
          
          if (resposta$status_code == 200) {
                    conteudo <- httr::content(resposta, as = 'text', encoding = 'UTF-8')
                    dados <- jsonlite::fromJSON(conteudo)
                    n.garantias <<- ifelse(is.null(nrow(dados)), 0, nrow(dados))
                    total.garantias <<- total.garantias + n.garantias
                    
                    if (n.garantias == 0) {
                              logger::log_warn('A consulta não retornou resultados para o Contrato {contrato}.
                         Desde o início da consulta, foram encontrados {total.garantias} garantias.')
                              
                              return(NULL)
                    } else {
                              logger::log_info(
                                        'Consulta bem-sucedida para o Contrato {id} de número {contrato}. Foram encontrados {n.garantias} garantias
          Desde o início da consulta, foram encontrados {total.garantias} garantias.'
                              )
                              
                              return(dados)
                    }
          } else {
                    logger::log_error(
                              'Ocorreu um erro na consulta do Contrato {contrato}. Código do erro: {resposta$status_code}.'
                    )
                    return(NULL)
          }
}

garantias.lista.consultar <- function() {

          contratos <- readr::read_rds('../rds/contratos.lista.dados.rds') |>
                    purrr::pluck(2) |> 
                    dplyr::select(numero, id)
          
          config <-
                    jsonlite::fromJSON('../configuracao/configuracao.json')
          simbolo <- acesso.API()
          cabecalho <- c(`accept` = 'application/json',
                         `Authorization` = paste0('Bearer ', simbolo))
          
          n.garantias <<- 0
          total.garantias <<- 0
          contratos$garantia <- vector("list", nrow(contratos))
          
          for (i in seq_along(contratos$id)) {
                    id <- contratos$id[i]
                    contrato <- contratos$numero[i]
                    garantia <- consultar.garantias(id, contrato, cabecalho)
                    contratos$garantia[i] <- list(garantia)
          }
          
          contratos.garantidos <- contratos |> 
                    dplyr::filter(garantia != 0)
          
          atualizacao <- Sys.Date()
          
          contratos.garantidos.dados <- list(atualizacao, contratos.garantidos)
          
          readr::write_rds(contratos.garantidos.dados, '../rds/contratos.garantidos.dados.rds')

}

garantias.lista.baixar <- function() {
          
          for (id in contratos.lista) {
                    contrato <- names(contratos.lista[contratos.lista == id])
                    contrato.garantido <- consultar.garantias(id, contrato, cabecalho)
                    contratos.garantidos <- rbind(contratos.garantidos, contrato.garantido)
          }
          
          
          df.contratos.resumido <-
                    contratos[contratos$id %in% contratos.garantidos$contrato_id, ] |> 
                    dplyr::select(id, numero, fornecedor_nome, tipo, categoria, processo, objeto, situacao)
          
          df.unificado <-
                    merge(
                              contratos.garantidos,
                              df.contratos.resumido,
                              by.x = 'contrato_id',
                              by.y = 'id',
                              all.x = TRUE,
                              suffixes = c('.garantia', '.contrato')
                    )
          
          df.unificado.ajustado <- df.unificado |>
                    dplyr::arrange(vencimento) |> 
                    dplyr::mutate(vencimento = format(as.Date(vencimento), format = "%d/%m/%Y"))
          
          
          garantias.lista <- data.frame(
                    Contrato = df.unificado.ajustado$numero,
                    Fornecedor = df.unificado.ajustado$fornecedor_nome,
                    Valor = df.unificado.ajustado$valor,
                    Garantia = df.unificado.ajustado$tipo.garantia,
                    Categoria = df.unificado.ajustado$categoria,
                    Vencimento = df.unificado.ajustado$vencimento
          )
          
          atualizacao <- Sys.Date()
          
          garantias.lista.dados <-
                    list(atualizacao, garantias.lista)
          
          
          saveRDS(garantias.lista.dados,
                  '../rds/garantias.lista.dados.rds')
          
}
