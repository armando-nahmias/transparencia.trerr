lista.recursos <- c(
          'arquivos',
          'garantias',
          'terceirizados',
          'cronograma',
          'despesas_acessorias',
          'domiciliobancario',
          'empenhos',
          'faturas',
          'historico',
          'itens',
          'ocorrencias',
          'prepostos',
          'publicacoes',
          'responsaveis'
)

for (recurso in lista.recursos) importar.recurso.comprasnet(recurso)




# tabela ------------------------------------------------------------------


formatado <- organizado |>
          DT::datatable(
                    caption = htmltools::tags$caption(
                              style = "caption-side: top; text-align: center;",
                              htmltools::HTML(paste0("Atualizado em ", atualizado))
                    ),
                    rownames = FALSE,
                    filter = "top",
                    class = "stripe",
                    options = list(
                              pageLength = 25,
                              autoWidth = TRUE,
                              scrollX = TRUE,
                              dom = 'Bfrtip',
                              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                              language = list(
                                        search = "Pesquisar:",
                                        info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
                                        lengthMenu = "Mostrar _MENU_ registros",
                                        paginate = list(
                                                  first = "Primeiro",
                                                  previous = "Anterior",
                                                  `next` = "Próximo",
                                                  last = "Último"
                                        ),
                                        zeroRecords = "Nenhum registro encontrado",
                                        infoEmpty = "Mostrando 0 a 0 de 0 registros",
                                        infoFiltered = "(filtrado de _MAX_ registros no total)"
                              ),
                              
                    )
          )

formatado
