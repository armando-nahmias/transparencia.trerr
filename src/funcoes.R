


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
                    jsonlite::fromJSON('configuracao/configuracao.json')
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


# Baixar tabela de terceirizados  -----------------------------------------

baixar.lista.terceirizados <- function() {
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
          
          destinatarios <-
                    list(Armando = 'armando.nahmias@tjrr.jus.br',
                         Adriana = 'adriana.schillreff@tre-rr.jus.br',
                         Jonilton = 'jonilton@tre-rr.jus.br')
          
          if (as.POSIXlt(Sys.Date())$wday != 1) {
                    destinatarios <- destinatarios[1]
          }
          
          tabela.terceirizados <-
                    readRDS('../rds/tabela.terceirizados.rds')
          contratos.terceirizados.modelo <-
                    readRDS('../rds/contratos.terceirizados.modelo.rds')
          
          
          # Instruções
          
          resultados <- list()
          total.terceirizados <<- 0
          n.terceirizados <<- 0
          
          lista.contratos.terceirizados.je <-
                    readRDS('rds/lista.contratos.terceirizados.rds')
          
          lista.contratos.terceirizados.trerr <-
                    lista.contratos.terceirizados.je |>
                    dplyr::filter(unidade_codigo == codigo.unidade, situacao == 'Ativo') |>
                    dplyr::select(1)
          
          
          for (i in seq_along(tabela.terceirizados)) {
                    Sys.sleep(.5)
                    
                    contrato <-
                              names(tabela.terceirizados[i])
                    id <-
                              tabela.terceirizados[[i]]
                    
                    terceirizados.consultado <-
                              consultar.terceirizados(id, contrato, cabecalho)
                    resultados[[length(resultados) + 1]] <-
                              terceirizados.consultado
          }
          
          if (length(resultados) > 0) {
                    df.terceirizados.consultado <- do.call(rbind, resultados)
                    
          } else {
                    cat('Nenhum terceirizado encontrado.\n')
          }
          
          df.contratos.resumido <-
                    lista.contratos.terceirizados.trerr[lista.contratos.terceirizados.trerr$id %in% df.terceirizados.consultado$contrato_id,]
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
          
          
          df.lista.final <- data.frame(
                    Contrato = df.unificado.ajustado$numero,
                    Fornecedor = df.unificado.ajustado$fornecedor_nome,
                    Terceirizado = df.unificado.ajustado$nome,
                    Função = df.unificado.ajustado$funcao,
                    Lotação = df.unificado.ajustado$unidade,
                    Jornada = df.unificado.ajustado$jornada
          )
          
          saveRDS(df.lista.final,
                  'rds/lista.terceirizados.rds')
          
          write.csv(df.lista.final,
                    'saida/lista.terceirizados.csv',
                    row.names = FALSE)
          anexos <-
                    file.path('saida',
                              list.files('saida', 'comprasnet.contratos.terceirizados'))
          
          # Construindo o assunto
          assunto <-
                    epoxy::epoxy('Relação terceirizados atualizada até {.data Sys.Date()}.')
          
          
          # Construindo o corpo
          corpo <- paste0(
                    epoxy::epoxy(
                              'Há {nrow(df.tabela.final)} terceirizados a incluir na relação do portal da transparência.'
                    )
          )
          
          enviar.mensagem(assunto, corpo, anexos, destinatarios)
          
          
          logger::log_warn(
                    'Término da execução da consulta ao comprasnet para baixar as informações sobre os terceirizados.'
          )
          
}


# Gerar tabela de terceirizados ------------------------------------

gerar.tabela.terceirizados <- function() {
          item <- '65.3'
          
          monitoramento <-
                    readr::read_rds('rds/monitoramento.rds')
          atualizacao <-
                    monitoramento$Atualizado[monitoramento$Item == item]
          
          dados <-
                    readr::read_rds('rds/lista.terceirizados.rds')
          
          ## conferir se os dados vêm ordenados e com o ano antes de excluir este comando
          dados <- dados |>
                    dplyr::mutate(Ano = stringr::str_match(Contrato, pattern = "\\/(\\d{4})")[, 2]) |>
                    dplyr::arrange(Fornecedor, Ano, Contrato)
          
          fornecedores <- unique(dados$Fornecedor)
          inicio.repeticao <-
                    which(!duplicated(dados[c('Fornecedor', 'Contrato')]))
          fim.repeticao <-
                    c(inicio.repeticao[-1] - 1, nrow(dados))
          rotulos <-
                    paste("Contrato:",
                          dados$Contrato[inicio.repeticao],
                          '- Fornecedor:',
                          dados$Fornecedor[inicio.repeticao])
          
          tabela <- dados |>
                    dplyr::select(-Fornecedor,-Contrato,-Ano) |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizacao)) |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
          for (i in seq_along(inicio.repeticao)) {
                    tabela <-
                              kableExtra::pack_rows(
                                        tabela,
                                        group_label = rotulos[i],
                                        start_row = inicio.repeticao[i],
                                        end_row = fim.repeticao[i]
                              )
          }
          
          
          readr::write_csv2(dados, 'saida/terceirizados.csv')
          
          kableExtra::save_kable(tabela, 'saida/terceirizados.pdf')
          
          kableExtra::save_kable(tabela, 'saida/terceirizados.html')
}


# ANP Preços médios -------------------------------------------------------

consultar.precos.medios.anp.semanal <- function() {
          endereco <- 'https://www.gov.br/anp/pt-br/assuntos/precos-e-defesa-da-concorrencia/precos/levantamento-de-precos-de-combustiveis-ultimas-semanas-pesquisadas'
          
          pagina <- rvest::read_html(endereco)
          
          referencia <- 'Preços médios semanais: Brasil, regiões, estados e municípios'
          
          fonte.dados <- pagina |> 
                    rvest::html_node(xpath = epoxy::epoxy('//a[contains(text(), "{referencia}")]')) |> 
                    rvest::html_attr('href')
          
          arquivo.dados.semanal <- '../dados/precos.medios.anp.semanal.xlsx'

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
          

          precos.medios.semanal.formatado <- precos.medios.semanal |> 
                    dplyr::filter(MUNICÍPIO %in% municipios, PRODUTO == produtos.semanal) |> 
                    dplyr::select(-6, -9, -11, -12) |> 
                    dplyr::rename(setNames(colunas.atuais.semanal, colunas.novas.semanal)) |> 
                    dplyr::mutate(
                              across(c(Inicio, Fim), ~ format(as.Date(as.numeric(.), origin = '1899-12-30'), format = "%d/%m/%Y")),
                              across(c(`Preço médio`, `Preço mínimo`), ~ sprintf("R$ %.2f", round(as.numeric(.), 2))),
                              across(c(`Preço médio`, `Preço mínimo`), ~stringr::str_replace_all(., "\\.", ","))
                    )
          
          atualizacao <- format(Sys.Date(), format = "%d/%m/%Y")
          
          precos.medios.semanal.dados <- list(atualizacao, precos.medios.semanal.formatado)
          
          readr::write_rds(precos.medios.semanal.dados,
                           '../rds/precos.medios.semanal.dados.rds')

          readr::write_csv2(precos.medios.semanal.formatado, '../saida/precos.medios.semanal.csv')

          tabela <- kableExtra::kable(precos.medios.semanal.formatado, format = 'html', align = 'lrr', caption = epoxy::epoxy('Atualizado em {atualizacao}'))
          
          kableExtra::save_kable(tabela, '../saida/precos.medios.semanal.pdf')
          
          kableExtra::save_kable(tabela, '../saida/precos.medios.semanal.html')

          
}
