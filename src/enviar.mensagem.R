enviar.mensagem <-
  function(assunto, corpo, anexos, DESTINATARIOS) {
    # assunto <- 'Novo Teste'
    # corpo <- 'Esta é uma mensagem de teste.'
    # anexos <- file.path('src', list.files('src', 'R'))
    # DESTINATARIOS <- 'armando.nahmias@tjrr.jus.br'
    # mensagem <- mensagem |> emayili::html(htmltools::h2('Relatorio'),htmltools::p(''))
    
    
    configuracao <- jsonlite::fromJSON('conf/configuracao.json')
    
    mensagem <- emayili::envelope(
      to = DESTINATARIOS,
      from = configuracao$usuario.smtp,
      subject = assunto,
      text = corpo
    )
    
    if (exists('anexos') && length(anexos) > 0) {
      for (anexo in anexos) {
        mensagem <- mensagem |> emayili::attachment(anexo)
      } 
      
      # mensagem <- lapply(anexos, function(anexo) {
      #   mensagem <<- mensagem |> emayili::attachment(anexo)
      # })[[1]] # Usamos [[1]] para pegar o envelope final após todos os anexos serem adicionados.
      
    } else {
      anexos <- NULL
    }
    
    configuracao.servidor <- emayili::server(
      host = "smtp.gmail.com",
      port = 465,
      username = configuracao$usuario.smtp,
      password = configuracao$senha.smtp
    )
    
    configuracao.servidor(mensagem)
    
    if (exists('mensagem')) {
      logger::log_info('Mensagem enviada com sucesso!')
    } else {
      logger::log_error('Falha ao enviar a mensagem.')

    }
  }
