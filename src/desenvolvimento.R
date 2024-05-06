

contratos.garantidos.dados <- readr::read_rds('../rds/contratos.garantidos.dados.rds')

contratos <- contratos.garantidos.dados |> 
          purrr::pluck(2)

cabecalho <- cabecalho.acesso.API()
n.garantias <- 0
total.garantias <- 0
contratos$garantia <- vector("list", nrow(contratos))


for (i in contratos$id[1]) {
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

# return(garantias.lista)

logger::log_warn(
          'Término da execução da consulta ao comprasnet para baixar as informações sobre as garantias.'
)


# tabela ------------------------------------------------------------------


garantias.lista.dados <- readr::read_rds('../rds/garantias.lista.dados.rds')

garantias.lista.atualizacao <- purrr::pluck(garantias.lista.dados, 1)


garantias.lista <- purrr::pluck(garantias.lista.dados, 2)

readr::write_csv2(garantias.lista,
                  '../saida/garantias.csv')

garantias.tabela <- garantias.lista |>
          kableExtra::kable(
                    format = 'html',
                    caption = epoxy::epoxy('Atualizado em {garantias.lista.atualizacao}')
          ) |>
          kableExtra::kable_styling(full_width = TRUE,
                                    bootstrap_options = 'striped')

kableExtra::save_kable(garantias.tabela,
                       '../saida/garantias.pdf')

kableExtra::save_kable(garantias.tabela,
                       '../saida/garantias.html')

