# Atualizando o monitoramento ---------------------------------------------

monitoramento <-
          readr::read_rds('00.diversos/monitoramento.transparencia.rds')

# monitoramento[87, 1] = 'GESTÃO DE PESSOAS'
# monitoramento[87, 2] = '65.3'
# monitoramento[87, 3] = 'Tabela de Terceirizados'

monitoramento <-
          readr::write_rds(monitoramento, '00.diversos/monitoramento.transparencia.rds')


# Incluindo a data dos itens monitorados ----------------------------------

monitoramento <-
          readr::read_rds('00.diversos/monitoramento.transparencia.rds')

atualizado <- lubridate::today()

monitoramento[87, 7] = lubridate::today()

monitoramento <-
          readr::write_rds(monitoramento, '00.diversos/monitoramento.transparencia.rds')

# Incluindo o endereço dos itens monitorados ----------------------------------

monitoramento <-
          readr::read_rds('00.diversos/monitoramento.transparencia.rds')

monitoramento[87, 8] = '08.pessoas/item.65.3.qmd'

monitoramento <-
          readr::write_rds(monitoramento, '00.diversos/monitoramento.transparencia.rds')



# Ajustando os formatos dos itens do monitoramento ------------------------


monitoramento$Item <- as.numeric(monitoramento$Item)
monitoramento$Atualizado <- lubridate::as_date(monitoramento$Atualizado)
