
# Importando os dados -----------------------------------------------------

source('../src/importar.R')

importar.contratos.anual.seges()

importar.precos.combustiveis()

lista.recursos <- c(
          'arquivos',
          'garantias',
          'terceirizados'
)

for (recurso in lista.recursos) importar.recurso.comprasnet(recurso)



# Tratando os dados -------------------------------------------------------

source('../src/organizar.R')

organizar.contratos()

organizar.terceirizados()

organizar.garantias()

organizar.arquivos()

# Formatando os dados -------------------------------------------------------

source('../src/formatar.R')

formatar.terceirizados()

formatar.contratos()

formatar.garantias()

formatar.combustiveis()