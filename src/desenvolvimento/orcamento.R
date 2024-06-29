# Função para mesclar e centralizar células repetidas
mesclar.centralizar <- function(coluna, inicio, valores) {
          estilo <- openxlsx::createStyle(halign = 'center', valign = 'center')
          for (linha in 6:(length(valores) + 4)) {
                    if (valores[linha - 4] != valores[linha - 5]) {
                              if (linha - inicio > 1) {
                                        openxlsx::mergeCells(wb, sheet = aba, cols = coluna, rows = inicio:(linha - 1))
                                        openxlsx::addStyle(wb, sheet = aba, style = estilo, cols = coluna, rows = inicio:(linha - 1), gridExpand = TRUE)
                              }
                              inicio <- linha
                    }
          }
          if (length(valores) + 5 - inicio > 1) {
                    openxlsx::mergeCells(wb, sheet = aba, cols = coluna, rows = inicio:(length(valores) + 4))
                    openxlsx::addStyle(wb, sheet = aba, style = estilo, rows = inicio:(length(valores) + 4), cols = coluna, gridExpand = TRUE)
          }
}

# Função para ajustar largura das colunas
ajustar.largura.colunas <- function(colunas.ajustadas, colunas.numericas, limite.largura) {
          for (coluna in seq_along(colunas.ajustadas)) {
                    if (colunas.ajustadas[coluna] %in% colunas.numericas) {
                              largura <- limite.largura / 2
                    } else {
                              max.largura <- max(nchar(as.character(orcamento[[coluna]])), na.rm = TRUE)
                              largura <- min(max.largura + 2, limite.largura)
                    }
                    openxlsx::setColWidths(wb, sheet = aba, cols = coluna, widths = largura)
          }
}

# Função para aplicar estilos às colunas
aplicar.estilos.colunas <- function(colunas.ajustadas, colunas.numericas, estilo.texto, estilo.moeda) {
          for (coluna in seq_along(colunas.ajustadas)) {
                    if (colunas.ajustadas[coluna] %in% colunas.numericas) {
                              openxlsx::addStyle(wb, sheet = aba, style = estilo.moeda, rows = 5:(nrow(orcamento) + 4), cols = coluna, gridExpand = TRUE)
                    } else {
                              openxlsx::addStyle(wb, sheet = aba, style = estilo.texto, rows = 5:(nrow(orcamento) + 4), cols = coluna, gridExpand = TRUE)
                    }
          }
}

# Configurando ------------------------------------------------------------
origem.dados <- 'dados/execucao.geral.txt'

colunas <- c("AG", "Ação Governo", "PO", "Plano Orçamentário", "GD", "Grupo Despesa", "ND", "Natureza Despesa Detalhada", "Nota de Empenho", "PI", "Plano Interno", "Favorecido", "Processo", "Despesas Empenhadas", "Despesas a Liquidar", "Despesas Pagas")
colunas.ajustadas <- colunas[c(-1, -3, -5, -7, -10)]
colunas.numericas <- c("Despesas Empenhadas", "Despesas a Liquidar", "Despesas Pagas")
titulo <- "Execução Orçamentária e Financeira Detalhada"
atualizacao <- format(Sys.Date() - 1, format = "%d/%m/%Y")
data.relatorio <- as.character(epoxy::epoxy("Atualizado até {atualizacao}."))
wb <- openxlsx::createWorkbook()
aba <- 'Execução Orçamentária'


# Importando --------------------------------------------------------------
orcamento <- readr::read_csv2(origem.dados, skip = 7, col_names = colunas, col_types = readr::cols(.default = 'c'), locale = readr::locale(decimal_mark = ',', grouping_mark = '.'))

# Organizando -------------------------------------------------------------
orcamento <- orcamento |> 
          tidyr::unite('Ação Governo', AG, `Ação Governo`, sep = ' - ', remove = TRUE) |> 
          tidyr::unite('Plano Orçamentário', PO, `Plano Orçamentário`, sep = ' - ', remove = TRUE) |> 
          tidyr::unite('Grupo Despesa', GD, `Grupo Despesa`, sep = ' - ', remove = TRUE) |> 
          tidyr::unite('Natureza Despesa Detalhada', ND, `Natureza Despesa Detalhada`, sep = ' - ', remove = TRUE) |> 
          tidyr::unite('Plano Interno', PI, `Plano Interno`, sep = ' - ', remove = TRUE) |> 
          dplyr::filter(`Ação Governo` != 'Total - NA', `Ação Governo` != 'AG - Ação Governo') |> 
          dplyr::mutate(
                    dplyr::across(dplyr::starts_with('Despesas'), ~ as.numeric(gsub(',', '.', gsub('\\.', '', .)))),
                    `Despesas Pagas` = tidyr::replace_na(`Despesas Pagas`, 0),
                    `Nota de Empenho` = stringr::str_match(`Nota de Empenho`, '.*(\\d{4}NE\\d{6})')[, 2]
          )

totais <- orcamento |> dplyr::summarise(dplyr::across(dplyr::starts_with('Despesas'), ~ sum(.x, na.rm = TRUE)))
linha.totais <- data.frame(c(rep('Total', 8), totais))
colnames(linha.totais) <- colnames(orcamento)
orcamento <- orcamento |> dplyr::add_row(linha.totais)

# Arquivo Excel -----------------------------------------------------------
openxlsx::addWorksheet(wb, sheetName = aba)
openxlsx::writeData(wb, sheet = aba, x = titulo, startRow = 1, startCol = 1)
openxlsx::writeData(wb, sheet = aba, x = data.relatorio, startRow = 3, startCol = 1)
openxlsx::writeData(wb, sheet = aba, x = as.data.frame(t(colunas.ajustadas)), startRow = 4, startCol = 1, colNames = FALSE)
openxlsx::writeData(wb, sheet = aba, x = orcamento, startRow = 5, startCol = 1, colNames = FALSE)

for (coluna in 1:3) {
          valores <- orcamento[[coluna]]
          mesclar.centralizar(coluna, 5, valores)
}

openxlsx::mergeCells(wb, sheet = aba, cols = 1:8, rows = (nrow(orcamento) + 4))
openxlsx::writeData(wb, sheet = aba, x = 'Totais', startRow = (nrow(orcamento) + 4), startCol = 1)

limite.largura <- 50
ajustar.largura.colunas(colunas.ajustadas, colunas.numericas, limite.largura)

estilo.cabecalho <- openxlsx::createStyle(fgFill = 'gray', halign = 'center', textDecoration = 'bold', valign = 'center')
openxlsx::addStyle(wb, sheet = aba, style = estilo.cabecalho, rows = 4, cols = 1:length(colunas.ajustadas), gridExpand = TRUE)

estilo.texto <- openxlsx::createStyle(fgFill = 'blue', fontColour = 'white', valign = 'center')
estilo.moeda <- openxlsx::createStyle(fgFill = 'lightblue', halign = 'right', numFmt = 'R$ #,##0.00', valign = 'center')
aplicar.estilos.colunas(colunas.ajustadas, colunas.numericas, estilo.texto, estilo.moeda)

openxlsx::setRowHeights(wb, sheet = aba, rows = c(1, 3, 4, (nrow(orcamento) + 4)), heights = 50)
openxlsx::setRowHeights(wb, sheet = aba, rows = 5:(nrow(orcamento) + 3), heights = 20)

vertical <- openxlsx::createStyle(valign = 'center')
openxlsx::addStyle(wb, sheet = aba, style = vertical, rows = 1:(nrow(orcamento) + 4), cols = 1:length(colunas.ajustadas), gridExpand = TRUE, stack = TRUE)

openxlsx::saveWorkbook(wb, "saida/Execucao_Orcamentaria.xlsx", overwrite = TRUE)

source('src/comunicar.R')

assunto <- 'Relatório de Execução Orçamentária'
corpo <- epoxy::epoxy('Relatório atualizado em {atualizacao}.')
anexos <- 'saida/Execucao_Orcamentaria.xlsx'
DESTINATARIOS <- 'nelsonamaro@tre-rr.jus.br'

enviar.mensagem(assunto, corpo, anexos, DESTINATARIOS)




