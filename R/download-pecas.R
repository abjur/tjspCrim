#' Funcao que faz o download das peticoes.
#'
#' @export
download_peticoes <- function(processos, login, senha, path) {
  rPython::python.assign('login', login)
  rPython::python.assign('senha', senha)
  rPython::python.load(system.file('python/todos_arquivos.py', package = 'tjsp'))

  for(i in 1:nrow(processos)) {
    try({
      rPython::python.call('pega_peticao',
                           as.character(processos[i, 'cd_processo']),
                           as.character(processos[i, 'n_processo']),
                           path)
    })
  }
  rPython::python.exec('driver.close()')
}

#' Funcao que faz o download de todos os arquivos
#'
#' @export
download_arqs <- function(processos, login, senha, path) {
  rPython::python.assign('login', login)
  rPython::python.assign('senha', senha)
  rPython::python.load(system.file('python/todos_arquivos.py', package = 'tjsp'))

  for(i in 1:nrow(processos)) {
    try({
      rPython::python.call('pega_todos_arquivos',
                           as.character(processos[i, 'cd_processo']),
                           as.character(processos[i, 'n_processo']),
                           path)
    })
  }
  rPython::python.exec('driver.close()')
}
