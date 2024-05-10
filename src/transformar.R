gerar.lista.contratos <- function() {
          destinos <- list.files(path = '../dados', pattern = 'seges.contratos.anual', full.names = TRUE)
          
          colunas <- destinos |> dplyr::last() |> readr::spec_csv()
          
          dados <- destinos |>
                    purrr::map_dfr(~readr::read_csv(.x, col_types = colunas), .id = "origem")
          
          lista.contratos <- dados |> 
                    dplyr::filter(unidade_codigo %in% c('070028', '70028')) |> 
                    dplyr::select(numero, id) |> 
                    dplyr::distinct()
          
          return(lista.contratos)
}
