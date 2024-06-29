

formatar.terceirizados <- function() {
          arquivo <- 'rds/terceirizados.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          if (nrow(organizado) == 0) return()
          
          formatado <- organizado |>
                    dplyr::select(-Contrato, -Fornecedor) |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado)) |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
          inicio.repeticao <-
                    which(!duplicated(organizado[c('Fornecedor', 'Contrato')]))
          fim.repeticao <-
                    c(inicio.repeticao[-1] - 1, nrow(organizado))
          rotulos <-
                    paste(
                              "Contrato:",
                              organizado$Contrato[inicio.repeticao],
                              '- Fornecedor:',
                              organizado$Fornecedor[inicio.repeticao]
                    )
          
          for (i in seq_along(inicio.repeticao)) {
                    formatado <-
                              kableExtra::pack_rows(
                                        formatado,
                                        group_label = rotulos[i],
                                        start_row = inicio.repeticao[i],
                                        end_row = fim.repeticao[i]
                              )
          }
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, 'saida/terceirizados.csv')
          
          kableExtra::save_kable(formatado, 'saida/terceirizados.pdf')
          
          kableExtra::save_kable(formatado, 'saida/terceirizados.html')
          
          return(formatado)
}

formatar.contratos <- function() {
          arquivo <- 'rds/contratos.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          
          colunas.atuais <-
                    c('numero',
                      'fornecedor_nome',
                      'tipo',
                      'categoria',
                      'objeto')
          colunas.novas <-
                    c('Contrato',
                      'Fornecedor',
                      'Tipo',
                      'Categoria',
                      'Objeto')
          
          formatado <- organizado |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'lllllllr') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
          formatado.interativo <- organizado |>
                    DT::datatable(
                              caption = htmltools::tags$caption(
                                        style = "caption-side: top; text-align: center;",
                                        htmltools::HTML(paste0("Atualizado em ", atualizado))
                              ),
                              options = list(
                                        pageLength = 25,
                                        autoWidth = TRUE,
                                        scrollX = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                        language = list(
                                                  search = "Pesquisar:",
                                                  info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
                                                  lengthMenu = "Mostrar _MENU_ registros",
                                                  paginate = list(
                                                            first = "Primeiro",
                                                            previous = "Anterior",
                                                            `next` = "Próximo",
                                                            last = "Último"
                                                  ),
                                                  zeroRecords = "Nenhum registro encontrado",
                                                  infoEmpty = "Mostrando 0 a 0 de 0 registros",
                                                  infoFiltered = "(filtrado de _MAX_ registros no total)"
                                        )
                              ),
                              rownames = FALSE,
                              filter = "top",
                              class = "stripe"
                    )
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, 'saida/contratos.csv')
          
          kableExtra::save_kable(formatado, 'saida/contratos.pdf')
          
          kableExtra::save_kable(formatado, 'saida/contratos.html')
          
          return(formatado.interativo)
}

formatar.garantias <- function() {
          arquivo <- 'rds/garantias.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          if (nrow(organizado) == 0) return()

          formatado <- organizado |>
                    dplyr::select(-Ano) |> 
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'lllrll') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
          inicio.repeticao <-
                    which(!duplicated(organizado['Ano']))
          fim.repeticao <-
                    c(inicio.repeticao[-1] - 1, nrow(organizado))
          rotulos <-
                    paste(
                              "Ano:",
                              organizado$Ano[inicio.repeticao]
                    )
          
          for (i in seq_along(inicio.repeticao)) {
                    formatado <-
                              kableExtra::pack_rows(
                                        formatado,
                                        group_label = rotulos[i],
                                        start_row = inicio.repeticao[i],
                                        end_row = fim.repeticao[i]
                              )
          }
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, 'saida/garantias.csv')
          
          kableExtra::save_kable(formatado, 'saida/garantias.pdf')
          
          kableExtra::save_kable(formatado, 'saida/garantias.html')
          
          return(formatado)
}

formatar.combustiveis <- function() {
          arquivo <- 'rds/combustiveis.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          municipio <- organizado |> 
                    dplyr::distinct(Municipio)
          estado <- organizado |> 
                    dplyr::distinct(Estado)
          
          formatado <- organizado |>
                    dplyr::select(-Estado, -Municipio) |> 
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'llllllrr') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c') |> 
                    kableExtra::footnote(general_title = 'Localidade: ',
                              general = paste0('Município: ', municipio, '; Estado: ', estado),
                              footnote_as_chunk = TRUE
                    )

          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, 'saida/combustiveis.csv')
          
          kableExtra::save_kable(formatado, 'saida/combustiveis.pdf')
          
          kableExtra::save_kable(formatado, 'saida/combustiveis.html')
          
          return(formatado)
}

formatar.arquivos <- function() {
          arquivo <- 'rds/arquivos.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          if (nrow(organizado) == 0) return()
          
          formatado <- organizado |>
                    dplyr::select(Tipo, `Descrição`, Objeto) |> 
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'llll') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c') |> 
                    kableExtra::column_spec(1, width = '10em') |> 
                    kableExtra::column_spec(2, width = '20em') |> 
                    kableExtra::column_spec(3, width = '50em') |> 
                    kableExtra::column_spec(2:3, link = organizado$Caminho)
          
          inicio.repeticao <-
                    which(!duplicated(organizado[c('Fornecedor', 'Contrato')]))
          fim.repeticao <-
                    c(inicio.repeticao[-1] - 1, nrow(organizado))
          rotulos <-
                    paste(
                              "Contrato:",
                              organizado$Contrato[inicio.repeticao],
                              '- Fornecedor:',
                              organizado$Fornecedor[inicio.repeticao]
                    )
          
          for (i in seq_along(inicio.repeticao)) {
                    formatado <-
                              kableExtra::pack_rows(
                                        formatado,
                                        group_label = rotulos[i],
                                        start_row = inicio.repeticao[i],
                                        end_row = fim.repeticao[i]
                              )
          }
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, 'saida/arquivos.csv')
          
          kableExtra::save_kable(formatado, 'saida/arquivos.pdf')
          
          kableExtra::save_kable(formatado, 'saida/arquivos.html')
          
          return(formatado)
}

formatar.pncp.atas <- function() {
          arquivo <- 'rds/pncp.atas.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          
          formatado <- organizado |>
                    dplyr::select(-Edital, -Ata) |> 
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'lllll') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c') |> 
                    kableExtra::column_spec(4, link = organizado$Edital) |> 
                    kableExtra::column_spec(2, link = organizado$Ata)
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, 'saida/atas.csv')
          
          kableExtra::save_kable(formatado, 'saida/atas.pdf')
          
          kableExtra::save_kable(formatado, 'saida/atas.html')
          
          return(formatado)
}

formatar.pncp.publicacao <- function(modalidade = 'pregao') {
          arquivo <- 'rds/pncp.publicacao.rds'
          dados <- readr::read_rds(arquivo)
          
          # modalidade = 'pregao'
          tipo = dplyr::case_when(
                    modalidade == 'pregao' ~ 'Pregão - Eletrônico',
                    modalidade == 'dispensabilidade' ~ 'Dispensa',
                    modalidade == 'inexigibilidade' ~ 'Inexigibilidade'
          )
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado') |> 
                    dplyr::filter(Modalidade == tipo)
          
          formatado <- organizado |>
                    dplyr::select(-Edital, -Acompanhar) |> 
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'llllrr') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c') |> 
                    kableExtra::column_spec(1:2, link = organizado$Edital) |> 
                    kableExtra::column_spec(4, link = organizado$Acompanhar)
          
          readr::write_csv2(organizado, epoxy::epoxy('saida/{modalidade}.csv'))
          
          kableExtra::save_kable(formatado, epoxy::epoxy('saida/{modalidade}.pdf'))
          
          kableExtra::save_kable(formatado, epoxy::epoxy('saida/{modalidade}.html'))
          
          return(formatado)
}

formatar.excel.orcamento <- function() {
          arquivo <- 'rds/execucao.detalhada.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          orcamento <- dados |> purrr::pluck('organizado')

          colunas <- c('Ação Governo', 'Plano Orçamentário', 'Grupo Despesa', 
                       'Natureza Despesa Detalhada', 'Nota de Empenho', 'Plano Interno', 
                       'Favorecido', 'Processo', 'Despesas Empenhadas', 'Despesas a Liquidar', 
                       'Despesas Pagas')
          titulo <- 'Execução Orçamentária e Financeira Detalhada'
          data.relatorio <- as.character(epoxy::epoxy('Atualizado até {atualizado}.'))
          wb <- openxlsx::createWorkbook()
          aba <- 'Execução Orçamentária'
          
          # Função para mesclar e centralizar células repetidas
          mesclar.centralizar <- function(coluna, inicio, valores) {
                    estilo <- openxlsx::createStyle(halign = 'center', valign = 'center')
                    for (linha in 6:(length(valores) + 4)) {
                              if (valores[linha - 4] != valores[linha - 5]) {
                                        if (linha - inicio > 1) {
                                                  openxlsx::mergeCells(wb, sheet = aba, cols = coluna, rows = inicio:(linha - 1))
                                                  openxlsx::addStyle(wb, sheet = aba, style = estilo, cols = coluna, rows = inicio:(linha - 1), gridExpand = TRUE)
                                        }
                                        inicio <- linha
                              }
                    }
                    if (length(valores) + 5 - inicio > 1) {
                              openxlsx::mergeCells(wb, sheet = aba, cols = coluna, rows = inicio:(length(valores) + 4))
                              openxlsx::addStyle(wb, sheet = aba, style = estilo, rows = inicio:(length(valores) + 4), cols = coluna, gridExpand = TRUE)
                    }
          }
          
          # Função para ajustar largura das colunas
          ajustar.largura.colunas <- function(colunas, limite.largura) {
                    for (coluna in seq_along(colunas)) {
                              if (startsWith(colunas[coluna], 'Despesas')) {
                                        largura <- limite.largura / 2
                              } else {
                                        max.largura <- max(nchar(as.character(orcamento[[coluna]])), na.rm = TRUE)
                                        largura <- min(max.largura + 2, limite.largura)
                              }
                              openxlsx::setColWidths(wb, sheet = aba, cols = coluna, widths = largura)
                    }
          }
          
          # Função para aplicar estilos às colunas
          aplicar.estilos.colunas <- function(colunas, estilo.texto, estilo.moeda) {
                    for (coluna in seq_along(colunas)) {
                              if (startsWith(colunas[coluna], 'Despesas')) {
                                        openxlsx::addStyle(wb, sheet = aba, style = estilo.moeda, rows = 5:(nrow(orcamento) + 4), cols = coluna, gridExpand = TRUE)
                              } else {
                                        openxlsx::addStyle(wb, sheet = aba, style = estilo.texto, rows = 5:(nrow(orcamento) + 4), cols = coluna, gridExpand = TRUE)
                              }
                    }
          }
          
          openxlsx::addWorksheet(wb, sheetName = aba)
          openxlsx::writeData(wb, sheet = aba, x = titulo, startRow = 1, startCol = 1)
          openxlsx::writeData(wb, sheet = aba, x = data.relatorio, startRow = 3, startCol = 1)
          openxlsx::writeData(wb, sheet = aba, x = as.data.frame(t(colunas)), startRow = 4, startCol = 1, colNames = FALSE)
          openxlsx::writeData(wb, sheet = aba, x = orcamento, startRow = 5, startCol = 1, colNames = FALSE)
          
          for (coluna in 1:3) {
                    valores <- orcamento[[coluna]]
                    mesclar.centralizar(coluna, 5, valores)
          }
          
          openxlsx::mergeCells(wb, sheet = aba, cols = 1:8, rows = (nrow(orcamento) + 4))
          openxlsx::writeData(wb, sheet = aba, x = 'Totais', startRow = (nrow(orcamento) + 4), startCol = 1)
          
          limite.largura <- 50
          ajustar.largura.colunas(colunas, limite.largura)
          
          estilo.cabecalho <- openxlsx::createStyle(fgFill = 'gray', halign = 'center', textDecoration = 'bold', valign = 'center')
          openxlsx::addStyle(wb, sheet = aba, style = estilo.cabecalho, rows = 4, cols = 1:length(colunas), gridExpand = TRUE)
          
          estilo.texto <- openxlsx::createStyle(fgFill = 'blue', fontColour = 'white', valign = 'center')
          estilo.moeda <- openxlsx::createStyle(fgFill = 'lightblue', halign = 'right', numFmt = 'R$ #,##0.00', valign = 'center')
          aplicar.estilos.colunas(colunas, estilo.texto, estilo.moeda)
          
          openxlsx::setRowHeights(wb, sheet = aba, rows = c(1, 3, 4, (nrow(orcamento) + 4)), heights = 50)
          openxlsx::setRowHeights(wb, sheet = aba, rows = 5:(nrow(orcamento) + 3), heights = 20)
          
          vertical <- openxlsx::createStyle(valign = 'center')
          openxlsx::addStyle(wb, sheet = aba, style = vertical, rows = 1:(nrow(orcamento) + 4), cols = 1:length(colunas), gridExpand = TRUE, stack = TRUE)
          
          openxlsx::saveWorkbook(wb, 'saida/Execucao_Orcamentaria.xlsx', overwrite = TRUE)

          return()
}
