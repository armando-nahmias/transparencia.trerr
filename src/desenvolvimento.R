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
cabecalho <- cabecalho.acesso.API()
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


contratos.garantidos.dados <- readr::read_rds('../rds/contratos.garantidos.dados.rds') |> 
          purrr::pluck(2)


contratos.garantidos <- readr::read_rds('../rds/contratos.garantidos.dados.rds') |> 
          purrr::pluck(2) |> 
          dplyr::select(-garantia) |> 
          dplyr::rename(contrato = id)



total.garantias <- 0
garantias <- data.frame()

for (i in seq_along(contratos.garantidos$contrato[1:5])) {
          id <- contratos.garantidos$contrato[i]
          contrato <- contratos.garantidos$numero[i]
          dados <- consultar.garantias(id, contrato, cabecalho)
          garantias <- rbind(garantias, dados)
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
str(garantias[[1]])

garantias <- contratos.garantidos |> purrr::pluck(3)

# Se todas as listas tiverem a mesma estrutura, você pode combinar todas usando do.call e rbind
df_garantias <- do.call(rbind, garantias)

# Transformando as linhas em um dataframe
df_garantias <- tibble::as_tibble(df_garantias)

# Verificando o dataframe resultante
head(df_garantias)

garantias <- merge(contratos.garantidos, df_garantias, by.x='id', by.y='contrato_id')

