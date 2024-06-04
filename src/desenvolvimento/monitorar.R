# Atualizando o monitoramento ---------------------------------------------

monitoramento <-
          readr::read_rds('rds/monitoramento.rds')

# monitoramento[87, 1] = 'GESTÃO DE PESSOAS'
# monitoramento[87, 2] = '65.3'
# monitoramento[87, 3] = 'Tabela de Terceirizados'

monitoramento <-
          readr::write_rds(monitoramento, 'rds/monitoramento.transparencia.rds')


# Incluindo a data dos itens monitorados ----------------------------------

monitoramento <-
          readr::read_rds('rds/monitoramento.transparencia.rds')

atualizado <- lubridate::today()

monitoramento$Atualizado[monitoramento$Item == 65.3] = NA

readr::write_rds(monitoramento, 'rds/monitoramento.rds')

# Incluindo o endereço dos itens monitorados ----------------------------------

monitoramento <-
          readr::read_rds('rds/monitoramento.rds')

monitoramento[66, 8] = 'terceirizados.qmd'
monitoramento[, 8] = 'pagina.em.construcao.qmd'

readr::write_rds(monitoramento, 'rds/monitoramento.rds')



# Ajustando os formatos dos itens do monitoramento ------------------------


monitoramento$Item <- as.numeric(monitoramento$Item)
monitoramento$Atualizado <- lubridate::as_date(monitoramento$Atualizado)


# Adicionando nova linha --------------------------------------------------


monitoramento <- monitoramento %>% 
          dplyr::add_row(Eixo = '6', 
                  Item = 47.1, 
                  Descrição = 'Preços Médios de Combustíveis', 
                  Fundamento = NA, 
                  Pontuação = 0, 
                  Unidade = 'SAD', 
                  Atualizado = as.Date('2024-05-01'), 
                  Endereço = 'anp.precos.praticados.qmd')