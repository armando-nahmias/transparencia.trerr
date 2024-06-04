comando <- 'cd ~/Documents/repositorio/transparencia.trerr && quarto publish gh-pages >> transparencia.log 2>&1'

cronR::cron_add(command = comando, at = '21:10')
