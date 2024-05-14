

consolidar.contratos.anual.seges <- function() {
          arquivo <- '../rds/contratos.rds'
          
          if (file.exists(arquivo)) {
                    dados <- readr::read_rds(arquivo)
                    
                    contratos <- dados |> purrr::pluck('contratos')
                    atualizado <- dados |> purrr::pluck('atualizado')
                    if (lubridate::week(lubridate::dmy(atualizado)) != lubridate::week(Sys.Date())) {
                              atualizar <- TRUE
                    }
          } else {
                    atualizar <- TRUE
          }
          
          if (exists('atualizar')) {
                    UNIDADE.CODIGO <- c('070028', '70028')
                    
                    destinos <- list.files(
                              path = '../dados',
                              pattern = 'seges.contratos.anual',
                              full.names = TRUE
                    )
                    
                    colunas <- destinos |> dplyr::last() |> readr::spec_csv()
                    
                    dados <- destinos |>
                              purrr::map_dfr( ~ readr::read_csv(.x, col_types = colunas),
                                              .id = "origem")
                    
                    contratos <- dados |>
                              dplyr::filter(unidade_codigo %in% UNIDADE.CODIGO) |>
                              dplyr::distinct(id, .keep_all = TRUE)
                    
                    atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
                    
                    dados <- list(atualizado = atualizado, contratos = contratos)
                    
                    readr::write_rds(dados, arquivo)
                    
          }
          
          
          return(dados)
}

organizar.contratos <- function() {
          arquivo <- '../rds/contratos.rds'
          
          dados <- consolidar.contratos.anual.seges()
          contratos <- dados |> purrr::pluck('contratos')

          tipo.instrumento <- c('Contrato', 'Carta Contrato', 'Empenho')
          
          organizado <- contratos |>
                    dplyr::filter(situacao == 'Ativo', tipo %in% tipo.instrumento) |>
                    dplyr::mutate(
                              ano = as.factor(lubridate::year(data_assinatura)),
                              tipo = as.factor(tipo),
                              categoria = as.factor(categoria),
                              modalidade = as.factor(modalidade),
                              valor = scales::dollar(valor_global, prefix = "R$ ", decimal.mark = ",", big.mark = ".", accuracy = 0.01),
                              vigencia = format(vigencia_fim, "%d/%m/%Y")
                    ) |>
                    dplyr::arrange(tipo, ano, numero) |> 
                    dplyr::select(
                              Número = numero,
                              Fornecedor = fornecedor_nome,
                              Tipo = tipo,
                              Categoria = categoria,
                              Processo = processo,
                              Modalidade = modalidade,
                              Vigência = vigencia,
                              Valor = valor
                    )
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

organizar.terceirizados <- function() {
          arquivo <- '../rds/terceirizados.rds'
          dados <- readr::read_rds(arquivo)
          consulta <- dados$consulta
          
          contratos <- readr::read_rds('../rds/contratos.rds') |>
                    purrr::pluck('contratos') |>
                    dplyr::select(contrato_id = id, numero, fornecedor_nome)
          
          consolidado <- dplyr::inner_join(contratos, consulta, by = 'contrato_id')
          
          organizado <- consolidado |>
                    dplyr::filter(situacao == 'Ativo') |> 
                    dplyr::mutate(
                              nome = sapply(strsplit(usuario, ' - '), '[[', 2),
                              ano = stringr::str_match(numero, pattern = "\\/(\\d{4})")[, 2],
                              funcao = ifelse(
                                        is.na(descricao_complementar),
                                        funcao_id,
                                        paste(funcao_id, descricao_complementar)
                              ),
                              funcao = toupper(funcao)
                    ) |>
                    dplyr::arrange(fornecedor_nome, ano, numero) |> 
                    dplyr::mutate(
                              Contrato = as.character(numero),
                              Fornecedor = as.character(fornecedor_nome),
                              Terceirizado = as.character(nome),
                              `Função` = as.factor(funcao),
                              `Lotação` = as.factor(unidade)
                    ) |> 
                    dplyr::select(Contrato, Fornecedor, Terceirizado, `Função`, `Lotação`)
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
}

organizar.garantias <- function() {
          arquivo <- '../rds/garantias.rds'
          dados <- readr::read_rds(arquivo)
          consulta <- dados$consulta
          
          contratos <- readr::read_rds('../rds/contratos.rds') |>
                    purrr::pluck('contratos') |>
                    dplyr::select(contrato_id = id, numero, fornecedor_nome)
          
          consolidado <- dplyr::inner_join(contratos, consulta, by = 'contrato_id')
          
          organizado <- consolidado |>
                    dplyr::mutate(
                              Contrato = as.character(numero),
                              Ano = stringr::str_match(numero, pattern = "\\/(\\d{4})")[, 2],
                              Fornecedor = as.character(fornecedor_nome),
                              Tipo = as.factor(tipo),
                              valor = stringr::str_remove_all(valor, '\\.'),
                              valor = as.numeric(stringr::str_replace_all(valor, ',', '.')),
                              Valor = scales::dollar(valor, prefix = "R$ ", decimal.mark = ",", big.mark = ".", accuracy = 0.01),
                              `Situação` = ifelse(vencimento < Sys.Date(), 'Expirada', 'Ativa'),
                              Vencimento = format(as.Date(vencimento), "%d/%m/%Y")
                    ) |> 
                    dplyr::distinct(contrato_id, .keep_all = TRUE) |> 
                    dplyr::arrange(Ano, Contrato) |> 
                    dplyr::select(Ano, Contrato, Fornecedor, Tipo, Valor, Vencimento, `Situação`)
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

organizar.arquivos <- function() {
          arquivo <- '../rds/arquivos.rds'
          dados <- readr::read_rds(arquivo)
          consulta <- dados$consulta

          contratos <- readr::read_rds('../rds/contratos.rds') |>
                    purrr::pluck('contratos') |>
                    dplyr::select(contrato_id = id, numero, fornecedor_nome, objeto)
          
          consolidado <- dplyr::inner_join(contratos, consulta, by = 'contrato_id')
          
          organizado <- consolidado |>
                    dplyr::arrange(contrato_id, id) |> 
                    dplyr::mutate(
                              Contrato = as.character(numero),
                              Fornecedor = as.character(fornecedor_nome),
                              Tipo = as.factor(tipo),
                              Objeto = as.character(objeto)
                    ) |> 
                    dplyr::rename(Processo = processo, `Descrição` = descricao, Caminho = path_arquivo) |> 
                    dplyr::select(Contrato, Fornecedor, Tipo, `Descrição`, Objeto, Caminho)
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
}

organizar.combustiveis <- function() {
          arquivo <- '../rds/combustiveis.rds'
          dados <- readr::read_rds(arquivo)
          
          consultado <- dados |> purrr::pluck('consultado')
          atualizado <- dados |> purrr::pluck('atualizado')
          
          organizado <- consultado |>
                    dplyr::mutate(
                              across(
                                        c(Inicio, Fim),
                                        ~ format(as.Date(as.numeric(.), origin = '1899-12-30'), format = "%d/%m/%Y")
                              ),
                              across(
                                        c(`Preço médio`, `Preço mínimo`),
                                        ~ sprintf("R$ %.2f", round(as.numeric(.), 2))
                              ),
                              across(
                                        c(`Preço médio`, `Preço mínimo`),
                                        ~ stringr::str_replace_all(., "\\.", ",")
                              ),
                              Fim = ifelse(
                                        is.na(Fim),
                                        format(lubridate::ceiling_date(as.Date(Inicio, format = "%d/%m/%Y"), "month") - lubridate::days(1), "%d/%m/%Y"),
                                        Fim
                              )
                    )

          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

