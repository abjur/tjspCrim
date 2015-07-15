#' Funcao que faz o download da lista de processos na Consulta de Julgados do Primeiro Grau (CJPG)
#' do TJSP.
#'
#' @export
list_cjpg <- function(livre = NULL, classes = NULL, assuntos = NULL,
                      data_inicial = NULL, data_final = NULL, varas = NULL,
                      min_pag = 1, max_pag = 10, salvar = FALSE, path = '') {
  url_pesq <- build_url_pesq(livre, classes, assuntos, data_inicial, data_final, varas)
  r <- httr::GET(url_pesq, config = list(ssl.verifypeer = FALSE))
  if(is.infinite(max_pag)) {
    h <- XML::htmlParse(httr::content(r, 'text'), encoding = 'UTF-8')
    val <- XML::xmlValue(XML::getNodeSet(h, "//*[@id='resultados']//td")[[1]])
    num <- as.numeric(stringr::str_match(stringr::str_trim(val), 'de ([0-9]+)')[1, 2])
    max_pag <- ceiling(num / 10)
    cat(sprintf('Paginas indo de %d a %d (total: %d)',
                min_pag, max_pag, max_pag - min_pag + 1), '\n')
  }
  d <- dplyr::bind_rows(lapply((min_pag):(max_pag), crawler_cjpg_pag, r = r,
                               salvar = salvar, path = path))
  return(d)
}

#' Calcula quantas paginas tem uma query no CJPG
#'
#' @export
n_cjpg <- function(livre = NULL, classes = NULL, assuntos = NULL,
                      data_inicial = NULL, data_final = NULL, varas = NULL) {
  url_pesq <- build_url_pesq(livre, classes, assuntos, data_inicial, data_final, varas)
  r <- httr::GET(url_pesq, config = list(ssl.verifypeer = FALSE))
  h <- XML::htmlParse(httr::content(r, 'text'), encoding = 'UTF-8')
  val <- XML::xmlValue(XML::getNodeSet(h, "//*[@id='resultados']//td")[[1]])
  num <- as.numeric(stringr::str_match(stringr::str_trim(val), 'de ([0-9]+)')[1, 2])
  num
}


parse_node_meta <- function(node) {
  val <- XML::xmlValue(node)
  val <- stringr::str_trim(stringr::str_split_fixed(gsub('[\n\r\t]','', val), ':', 2))
  df <- data.frame(val[1, 2], stringsAsFactors = FALSE)
  names(df) <- val[1, 1]
  df
}

parse_node <- function(node, salvar = FALSE, path = '', pag) {
  children <- XML::xmlChildren(node)
  df <- do.call(cbind, lapply(children[2:(length(children) - 1)], parse_node_meta))
  a <- XML::xmlChildren(XML::xmlChildren(children[[1]])$td)$a
  df$n_processo <- gsub('[\n\r\t ]', '', XML::xmlValue(a))
  df$cod_sentenca <- XML::xmlGetAttr(a, 'name')
  child_td <- XML::xmlChildren(XML::xmlChildren(children[[length(children)]])$td)[[4]]
  df$txt <- gsub('[\r\t]', '', XML::xmlValue(child_td))
  df$pag <- pag
  if(salvar) {
    n_processo_num <- gsub('[^0-9]', '', df$n_processo)
    arq <- sprintf('%s/%s_%s.rds', path, n_processo_num, df$cod_sentenca)
    if(!file.exists(arq)) {
      saveRDS(df, file = arq)
    }
  }
  df
}

crawler_cjpg_pag <- function(pag, r, salvar = FALSE, path = '') {
  url_pag <- sprintf('https://esaj.tjsp.jus.br/cjpg/trocarDePagina.do?pagina=%d', pag)
  cat(sprintf('pag: %d...', pag))
  cat('downloading...')
  r_pag <- httr::GET(url_pag,
                     config = c(ssl.verifypeer = FALSE,
                                httr::set_cookies(unlist(r$cookies))))
  cat('download realizado! ')
  try ({
    html <- XML::htmlParse(httr::content(r_pag, 'text'), encoding='UTF-8')
    nodes <- XML::getNodeSet(html, "//tr[@class='fundocinza1']//table")
    cat('parsing...')
    df <- dplyr::bind_rows(lapply(nodes, parse_node, salvar = salvar,
                                  path = path, pag = pag))
    df$pag <- pag
    names(df) <- gsub(' +', '_', tolower(rm_accent(names(df))))
    cat('OK!\n')
    return(df)
  }, TRUE)
  cat('ERROR !!!\n')
  return(data.frame())
}

#' @export
build_url_pesq <- function(livre = NULL, classes = NULL, assuntos = NULL,
                           data_inicial = NULL, data_final = NULL, varas = NULL) {
  base_url <- 'https://esaj.tjsp.jus.br/cjpg/pesquisar.do?dadosConsulta.nuProcesso='
  if(!is.null(livre)) {
    cod_livre <- paste0('&dadosConsulta.pesquisaLivre=', livre)
    base_url <- paste0(base_url, cod_livre)
  }
  if(!is.null(classes)) {
    cod_classes <- paste0('&classeTreeSelection.values=', paste(classes, collapse = '%2C'))
    base_url <- paste0(base_url, cod_classes)
  }
  if(!is.null(assuntos)) {
    cod_assuntos <- paste0('&assuntoTreeSelection.values=', paste(assuntos, collapse = '%2C'))
    base_url <- paste0(base_url, cod_assuntos)
  }
  if(!is.null(data_inicial) & !is.null(data_final)) {
    cod_data_inicial <- paste(lubridate::day(data_inicial),
                              lubridate::month(data_inicial),
                              lubridate::year(data_inicial) ,
                              sep = '%2F')
    cod_data_final <- paste(lubridate::day(data_final),
                            lubridate::month(data_final),
                            lubridate::year(data_final) ,
                            sep = '%2F')
    cod_data_inicial <- paste0('&dadosConsulta.dtInicio=', cod_data_inicial)
    cod_data_final <- paste0('&dadosConsulta.dtFim=', cod_data_final)
    base_url <- paste0(base_url, cod_data_inicial)
    base_url <- paste0(base_url, cod_data_final)
  }
  if(!is.null(varas)) {
    cod_varas <- paste0('&varasTreeSelection.values=', paste(varas, collapse = '%2C'))
    base_url <- paste0(base_url, cod_varas)
  }
  base_url
}

#' @export
carrega_pasta <- function(path) {
  l <- list.files(path, full.names = TRUE)
  d <- dplyr::data_frame(l = l)
  d <- dplyr::do(dplyr::group_by(d, l), readRDS(.$l))
  d <- dplyr::ungroup(d)
  d$l <- NULL
  d
}
