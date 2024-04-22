# setwd("~/Documents/repositorio/transparencia.trerr/08.pessoas")

item <- '65.3'

monitoramento <-
          readr::read_rds('../00.diversos/monitoramento.transparencia.rds')
atualizacao <- monitoramento$Atualizado[monitoramento$Item == item]

dados <-
          readr::read_rds('../08.pessoas/item.65.3.terceirizados.rds')

## conferir se os dados vêm ordenados e com o ano antes de excluir este comando
dados <- dados |>
          dplyr::mutate(Ano = stringr::str_match(Contrato, pattern = "\\/(\\d{4})")[,2]) |> 
          dplyr::arrange(Fornecedor, Ano, Contrato)

fornecedores <- unique(dados$Fornecedor)
inicio.repeticao <-
          which(!duplicated(dados[c('Fornecedor', 'Contrato')]))
fim.repeticao <- c(inicio.repeticao[-1] - 1, nrow(dados))
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


readr::write_csv2(dados, 'item.65.3.terceirizados.csv')

kableExtra::save_kable(tabela, 'item.65.3.terceirizados.pdf')

kableExtra::save_kable(tabela, 'item.65.3.terceirizados.html')
