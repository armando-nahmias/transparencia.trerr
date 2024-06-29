source('src/importar.R')
source('src/organizar.R')

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

importar.pncp.recursos()

importar.pncp.publicacao()

importar.execucao.detalhada()

