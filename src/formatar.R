

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
          
          formatado <- organizado |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'llllllrr') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')

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
