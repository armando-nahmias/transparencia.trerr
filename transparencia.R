source('../src/importar.R')
source('../src/organizar.R')

# Importando os dados -----------------------------------------------------

importar.contratos.anual.seges()

consolidar.contratos.anual.seges(atualizar = FALSE)

importar.contrato.comprasnet()

lista.recursos <- c(
          'arquivos',
          'garantias',
          'terceirizados'
)

for (recurso in lista.recursos) importar.recurso.comprasnet(recurso)

importar.precos.combustiveis()
# 
# # Tratando os dados -------------------------------------------------------
# 
# source('../src/organizar.R')
# 
# organizar.contratos()
# 
# organizar.terceirizados()
# 
# organizar.garantias()
# 
# organizar.arquivos()
# 
# organizar.combustiveis()
# 
# # Formatando os dados -------------------------------------------------------
# 
# source('../src/formatar.R')
# 
# formatar.terceirizados()
# 
# formatar.contratos()
# 
# formatar.garantias()
# 
# formatar.combustiveis()
# 
# formatar.arquivos()