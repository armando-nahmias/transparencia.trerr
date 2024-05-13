

# formatar ----------------------------------------------------------------



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



# semanal -----------------------------------------------------------------





precos.medios.semanal.formatado <-
          precos.medios.semanal |>

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

