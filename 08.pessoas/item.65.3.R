
dados <-
          readr::read_rds('../08.pessoas/item.65.3.terceirizados.rds')
fornecedores <- unique(dados$Fornecedor)

tabela <- dados |>
          dplyr::relocate(Fornecedor, .before = Contrato) |>
          kableExtra::kbl() |>
          kableExtra::kable_styling(bootstrap_options = 'stripe') |>
          kableExtra::row_spec(0, align = 'c') |>
          kableExtra::collapse_rows(columns = 1:2, )


readr::write_csv2(dados, 'item.65.3.terceirizados.csv')

kableExtra::save_kable(tabela, 'item.65.3.terceirizados.pdf')
