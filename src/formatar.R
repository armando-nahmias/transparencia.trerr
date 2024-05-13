

formatar.terceirizados <- function() {
          arquivo <- '../rds/terceirizados.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          
          fornecedores <- unique(organizado$Fornecedor)
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
          
          formatado <- organizado |>
                    dplyr::select(-Contrato, -Fornecedor) |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado)) |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
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
          
          readr::write_csv2(organizado, '../saida/terceirizados.csv')
          
          kableExtra::save_kable(formatado, '../saida/terceirizados.pdf')
          
          kableExtra::save_kable(formatado, '../saida/terceirizados.html')
          
          return(formatado)
}

formatar.contratos <- function() {
          arquivo <- '../rds/contratos.rds'
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
                    kableExtra::kable(
                              format = 'html',
                              caption = epoxy::epoxy('Atualizado em {atualizado}')
                    ) |>
                    kableExtra::kable_styling(full_width = TRUE,
                                              bootstrap_options = 'striped')
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, '../saida/contratos.csv')
          
          kableExtra::save_kable(formatado, '../saida/contratos.pdf')
          
          kableExtra::save_kable(formatado, '../saida/contratos.html')
          
          return(formatado)
}

formatar.garantias <- function() {
          arquivo <- '../rds/garantias.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          organizado <- dados |> purrr::pluck('organizado')
          
          formatado <- organizado |>
                    kableExtra::kbl(caption = paste0('Dados atualizados em ', atualizado),
                                    align = 'lllrll') |>
                    kableExtra::kable_styling(bootstrap_options = 'stripe') |>
                    kableExtra::row_spec(0, align = 'c')
          
          
          dados$formatado <- formatado
          readr::write_rds(dados, arquivo)
          
          readr::write_csv2(organizado, '../saida/garantias.csv')
          
          kableExtra::save_kable(formatado, '../saida/garantias.pdf')
          
          kableExtra::save_kable(formatado, '../saida/garantias.html')
          
          return(formatado)
}

formatar.combustiveis <- function() {
          arquivo <- '../rds/combustiveis.rds'
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
          
          readr::write_csv2(organizado, '../saida/combustiveis.csv')
          
          kableExtra::save_kable(formatado, '../saida/combustiveis.pdf')
          
          kableExtra::save_kable(formatado, '../saida/combustiveis.html')
          
          return(formatado)
}
