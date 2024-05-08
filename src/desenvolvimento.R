contratos.garantidos.dados <- readr::read_rds('../rds/contratos.garantidos.dados.rds') |> 
          purrr::pluck(2)


garantias <- readr::read_rds('../rds/contratos.garantidos.dados.rds') |> 
          purrr::pluck(2) |> 
          dplyr::select(-garantia)


cabecalho <- cabecalho.acesso.API()


garantias$id <- vector("list", nrow(garantias))
garantias$tipo <- vector("list", nrow(garantias))
garantias$valor <- vector("list", nrow(garantias))
garantias$vencimento <- vector("list", nrow(garantias))

for (i in seq_along(garantias$id)) {
          id <- garantias$id[i]
          contrato <- garantias$numero[i]
          dados <- consultar.garantias(id, contrato, cabecalho)
          for (dado in dados) {
                    print(dado)
          }
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


# teste -------------------------------------------------------------------


# Supondo que lista.garantias seja a sua lista de listas
# Primeiro, verifique se todas as listas têm a mesma estrutura
# Isso é importante para garantir que o rbind não encontrará problemas

# Verificando a estrutura da primeira lista como exemplo
str(lista.garantias[[1]])

# Se todas as listas tiverem a mesma estrutura, você pode combinar todas usando do.call e rbind
df_garantias <- do.call(rbind, lista.garantias)

# Transformando as linhas em um dataframe
df_garantias <- tibble::as_tibble(df_garantias)

# Verificando o dataframe resultante
head(df_garantias)

garantias <- merge(contratos.garantidos, df_garantias, by.x='id', by.y='contrato_id')

