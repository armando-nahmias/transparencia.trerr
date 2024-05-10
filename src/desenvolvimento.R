lista.recursos <- c(
          'arquivos',
          # 'empenhos',
          'faturas',
          'garantias',
          # 'itens',
          'publicacoes',
          'terceirizados'
)

for (recurso in lista.recursos) consultar.recurso(recurso)
