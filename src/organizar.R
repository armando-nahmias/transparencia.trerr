organizar.contratos <- function() {
          arquivo <- 'rds/contratos.rds'
          
          dados <- consolidar.contratos.anual.seges()
          contratos <- dados |> purrr::pluck('consultado')

          tipo.instrumento <- c('Contrato', 'Carta Contrato', 'Empenho')
          
          organizado <- contratos |>
                    dplyr::filter(situacao == 'Ativo', tipo %in% tipo.instrumento) |>
                    dplyr::mutate(
                              ano = as.factor(lubridate::year(data_assinatura)),
                              tipo = as.factor(tipo),
                              categoria = as.factor(categoria),
                              modalidade = as.factor(modalidade),
                              valor_global = stringr::str_remove_all(valor_global, '\\.'),
                              valor_global = as.numeric(stringr::str_replace_all(valor_global, ',', '.')),
                              valor_global = scales::dollar(valor_global, prefix = "R$ ", decimal.mark = ",", big.mark = ".", accuracy = 0.01),
                              vigencia = format(as.Date(vigencia_fim), "%d/%m/%Y")
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
                              Valor = valor_global
                    )
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

organizar.terceirizados <- function() {
          arquivo <- 'rds/terceirizados.rds'
          dados <- readr::read_rds(arquivo)
          consulta <- dados$consulta
          
          contratos <- readr::read_rds('rds/contratos.rds') |>
                    purrr::pluck('consultado') |>
                    dplyr::select(contrato_id = id, numero, fornecedor_nome)
          
          consolidado <- dplyr::inner_join(contratos, consulta, by = 'contrato_id')
          
          organizado <- consolidado |>
                    # dplyr::filter(situacao == 'Ativo') |>
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
          arquivo <- 'rds/garantias.rds'
          dados <- readr::read_rds(arquivo)
          consulta <- dados$consulta
          
          contratos <- readr::read_rds('rds/contratos.rds') |>
                    purrr::pluck('consultado') |>
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
                              Vencimento = format(as.Date(vencimento), "%d/%m/%Y")
                    ) |> 
                    dplyr::distinct(contrato_id, .keep_all = TRUE) |> 
                    dplyr::arrange(Ano, Contrato) |> 
                    dplyr::select(Ano, Contrato, Fornecedor, Tipo, Valor, Vencimento)
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

organizar.arquivos <- function() {
          arquivo <- 'rds/arquivos.rds'
          dados <- readr::read_rds(arquivo)
          consulta <- dados$consulta

          contratos <- readr::read_rds('rds/contratos.rds') |>
                    purrr::pluck('consultado') |>
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
          arquivo <- 'rds/combustiveis.rds'
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

organizar.pncp.atas <- function() {
          arquivo <- 'rds/pncp.atas.rds'
          dados <- readr::read_rds(arquivo)
          
          consultado <- dados |> purrr::pluck('consultado')
          atualizado <- dados |> purrr::pluck('atualizado')
          
          organizado <- consultado |>
                    dplyr::distinct(numeroControlePNCPAta, .keep_all = TRUE) |> 
                    dplyr::mutate(
                              dplyr::across(c("anoAta", "usuario", "codigoUnidadeOrgao", "nomeUnidadeOrgao"), as.factor),
                              dplyr::across(c("dataCancelamento", "dataAssinatura", "vigenciaInicio", "vigenciaFim", "dataPublicacaoPncp", "dataInclusao", "dataAtualizacao"),
                                            ~ format(as.Date(.), format = "%d/%m/%Y")),
                              dplyr::across(c("cnpjOrgaoSubrogado", "nomeOrgaoSubrogado", "codigoUnidadeOrgaoSubrogado", "nomeUnidadeOrgaoSubrogado"), as.character),
                              anoContratacao = stringr::str_extract(numeroControlePNCPAta, "(?<=/)\\d{4}(?=-)"),
                              controlePNCP = stringr::str_extract(numeroControlePNCPAta, "(?<=-)\\d{6}(?=/)"),
                              controleARP = stringr::str_extract(numeroControlePNCPAta, "(?<=-)\\d+$"),
                              edital = stringr::str_c("https://pncp.gov.br/app/editais/", cnpjOrgao, "/", anoContratacao, "/", controlePNCP),
                              ata = stringr::str_replace(edital, "/editais/", "/atas/") |>
                                        stringr::str_c('/', controleARP),
                              situacao = ifelse(vigenciaInicio > as.Date('2024-05-20'), 'Ativo', 'Expirado'),
                              licitacao = 'Licitação'
                    ) |>
                    dplyr::select(Ano = anoAta,
                                  `Número` = numeroAtaRegistroPreco,
                                  `Vigência` = vigenciaFim,
                                  `Licitação` = licitacao,
                                  Objeto = objetoContratacao,
                                  Edital = edital,
                                  Ata = ata) |> 
                    dplyr::arrange(Ano, `Número`)
          

          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

organizar.pncp.proposta <- function() {
          arquivo <- 'rds/pncp.proposta.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          consultado <- dados |> purrr::pluck('consultado')
          if (nrow(consultado) == 0) return(consultado)
          
          organizado <- consultado
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

organizar.pncp.publicacao <- function() {
          arquivo <- 'rds/pncp.publicacao.rds'
          dados <- readr::read_rds(arquivo)
          
          atualizado <- dados |> purrr::pluck('atualizado')
          consultado <- dados |> purrr::pluck('consultado')
          if (nrow(consultado) == 0) return(consultado)
          
          organizado <- consultado |> 
                    dplyr::mutate(
                              anoCompra = as.factor(anoCompra),
                              valorTotalHomologado = scales::dollar(valorTotalHomologado, prefix = "R$ ", decimal.mark = ",", big.mark = ".", accuracy = 0.01),
                              valorTotalEstimado = scales::dollar(valorTotalEstimado, prefix = "R$ ", decimal.mark = ",", big.mark = ".", accuracy = 0.01),
                              cnpjOrgao = stringr::str_extract(numeroControlePNCP, "\\d{14}"),
                              controlePNCP = stringr::str_extract(numeroControlePNCP, "(?<=-)(\\d{6})"),
                              anoContratacao = stringr::str_extract(numeroControlePNCP, "\\d{4}$"),
                              edital = stringr::str_c("https://pncp.gov.br/app/editais/", cnpjOrgao, "/", anoContratacao, "/", controlePNCP),
                              edital = htmltools::htmlEscape(edital),
                              linkSistemaOrigem = htmltools::htmlEscape(linkSistemaOrigem)
                    ) |> 
                    dplyr::select(
                              Ano = anoCompra,
                              `Número` = numeroCompra,
                              Modalidade = modalidadeNome,
                              Objeto = objetoCompra,
                              Estimado = valorTotalEstimado,
                              Homologado = valorTotalHomologado,
                              Edital = edital,
                              `Acompanhar` = linkSistemaOrigem
                    ) |> 
                    dplyr::arrange(Ano, `Número`)
          
          dados$organizado <- organizado
          
          readr::write_rds(dados, arquivo)
          
          return(organizado)
          
}

# Funções secundárias -----------------------------------------------------
consolidar.contratos.anual.seges <- function(atualizar = FALSE) {
          arquivo <- 'rds/contratos.rds'
          
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
          
          if (atualizar == TRUE) {
                    UNIDADE.CODIGO <- readr::read_rds('configuracao/unidade.codigo.rds')
                    
                    destinos <- list.files(
                              path = 'dados',
                              pattern = 'seges.contratos.anual',
                              full.names = TRUE
                    )
                    
                    colunas <- destinos |> dplyr::last() |> readr::spec_csv()
                    
                    dados <- destinos |>
                              purrr::map_dfr( ~ readr::read_csv(.x, col_types = colunas),
                                              .id = "origem")
                    
                    contratos <- dados |>
                              dplyr::filter(unidade_codigo %in% UNIDADE.CODIGO) |>
                              dplyr::distinct(id, numero)
                    
                    atualizado <- format(Sys.Date(), format = '%d/%m/%Y')
                    
                    dados <- list(atualizado = atualizado, contratos = contratos)
                    
                    readr::write_rds(dados, arquivo)
                    
          }
          
          return(dados)
}

