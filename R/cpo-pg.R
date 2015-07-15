#' @export
parse_cpo_pg <- function(path, sample = NULL, keyval = FALSE) {
  l <- list.files(path, full.names = TRUE)
    if(!is.null(sample)) {
    l <- sample(l, sample, replace = FALSE)
  }
  d <- dplyr::data_frame(l = l)
  d <- dplyr::group_by(d, l)

  if(keyval) {
    d <- dplyr::do(d, parse_cpo_pg_um_keyval(.$l))
  } else {
    d <- dplyr::do(d, parse_cpo_pg_um(.$l))
  }
  d <- dplyr::ungroup(d)
  d <- dplyr::select(d, -l)
  d
}

#' @export
parse_cpo_pg_um_keyval <- function(r) {
  try({
    h <- rvest::html(r, encoding = "UTF-8")
    xpath <- "//table[@id != 'secaoFormConsulta' and (@class='secaoFormBody' "
    xpath <- paste0(xpath, "or @id='tableTodasPartes' or (@id='tablePartes")
    xpath <- paste0(xpath, "Principais' and @id!='tableTodasPartes'))]//tr//td")
    keyval <- sapply(XML::getNodeSet(h, xpath), XML::xmlValue)
    keyval <- iconv(keyval, to = 'UTF-8')
    keyval <- gsub('às [0-9]+\\:[0-9]+', '', keyval)
    keyval <- stringr::str_trim(gsub('\\&nbsp', ' ', keyval))
    keyval <- stringr::str_trim(gsub(" +", " ", gsub("[ \t\r\n\v\f]+", " ", keyval)))
    keyval <- keyval[!duplicated(keyval, incomparables = '') | stringr::str_detect(keyval, ':[^[:alpha:]]*$')]
    keyval <- paste(keyval, collapse = ' ')

    re <- '(([[:alpha:]]+:)|(Valor da ação:)|(Outros assuntos:)|(Local Físico:))'
    key <- stringr::str_match_all(keyval, re)[[1]][, 2]
    key <- stringr::str_trim(gsub(':', '', key))
    key <- rm_accent(gsub(' +', '_', tolower(key)))
    key[key %in% c('reqte', 'reclamante')] <- 'reqte'
    key[key %in% c('reqda', 'reclamada', 'reclamado')] <- 'reqdo'
    key[key == 'advogada'] <- 'advogado'

    val <- stringr::str_split(keyval, re)[[1]][-1]
    val <- stringr::str_trim(gsub('^[^A-Za-z0-9]+|[^A-Za-z0-9]+$', '', val))

    if(any(stringr::str_detect(key, 'reqte')) &
       any(stringr::str_detect(key, 'reqdo')) &
       any(stringr::str_detect(key, 'adv'))) {
      ind <- 1:length(key) %in% (which(key == 'reqte')[1] + 1):(which(key == 'reqdo')[1] - 1)
      ind <- ind & (key == 'advogado')
      key[ind] <- 'reqte_adv'
      val[ind] <- paste(val[ind], collapse = '\n')
      val[key == 'advogado'] <- paste(val[key == 'advogado'], collapse = '\n')
      key[key == 'advogado'] <- 'reqdo_adv'
    }

    d <- data.frame(key, val, stringsAsFactors = FALSE)
    if(is.character(r)) {
      d$arq <- r
    }
    return(d)
  })
  if(is.character(r)) {
    d <- data.frame(arq = r, stringsAsFactors = FALSE)
  } else {
    d <- data.frame()
  }
  return(d)
}

#' @export
valor_acao <- function(x) {
  as.numeric(gsub('[^0-9.]', '', gsub(',', '.', gsub('\\.', '', x))))
}

#' @export
parse_cpo_pg_um <- function(r) {
  try({
    h <- rvest::html(r, encoding = "UTF-8")
    xpath <- "//table[@id != 'secaoFormConsulta' and (@class='secaoFormBody' "
    xpath <- paste0(xpath, "or @id='tableTodasPartes' or (@id='tablePartes")
    xpath <- paste0(xpath, "Principais' and @id!='tableTodasPartes'))]//tr//td")
    keyval <- sapply(XML::getNodeSet(h, xpath), XML::xmlValue)
    keyval <- iconv(keyval, to = 'UTF-8')
    keyval <- gsub('às [0-9]+\\:[0-9]+', '', keyval)
    keyval <- stringr::str_trim(gsub('\\&nbsp', ' ', keyval))
    keyval <- stringr::str_trim(
      gsub(" +", " ", gsub("[ \t\r\n\v\f]+", " ", keyval))
    )
    keyval <- keyval[!duplicated(keyval, incomparables = '') |
                       stringr::str_detect(keyval, ':[^[:alpha:]]*$')]
    keyval <- paste(keyval, collapse = ' ')

    re <- '(([[:alpha:]]+:)|(Valor da ação:)|(Outros assuntos:)|(Local Físico:))'
    key <- stringr::str_match_all(keyval, re)[[1]][, 2]
    key <- stringr::str_trim(gsub(':', '', key))
    key <- rm_accent(gsub(' +', '_', tolower(key)))
    key[key %in% c('reqte', 'reclamante')] <- 'reqte'
    key[key %in% c('reqda', 'reclamada', 'reclamado')] <- 'reqdo'
    key[key == 'advogada'] <- 'advogado'

    val <- stringr::str_split(keyval, re)[[1]][-1]
    val <- stringr::str_trim(gsub('^[^A-Za-z0-9]+|[^A-Za-z0-9]+$', '', val))

    if(any(stringr::str_detect(key, 'reqte')) &
       any(stringr::str_detect(key, 'reqdo')) &
       any(stringr::str_detect(key, 'adv'))) {
      ind <- 1:length(key) %in%
        (which(key == 'reqte') + 1):(which(key == 'reqdo') - 1)
      ind <- ind & (key == 'advogado')
      key[ind] <- 'reqte_adv'
      val[ind] <- paste(val[ind], collapse = '\n')
      val[key == 'advogado'] <- paste(val[key == 'advogado'], collapse = '\n')
      key[key == 'advogado'] <- 'reqdo_adv'
    }

    d <- data.frame(key, val, stringsAsFactors = FALSE)
    d <- dplyr::distinct(d)
    d <- dplyr::summarise(dplyr::group_by(d, key),
                          val = paste(val, collapse = '\n'))
    d$aux <- 1
    d <- tidyr::spread(d, key, val)
    d <- dplyr::select(d, -aux)
    d$valor_da_acao <- ifelse(
      is.null(d$valor_da_acao), NA,
      as.numeric(gsub('[^0-9.]', '',
                      gsub(',', '.', gsub('\\.', '', d$valor_da_acao)))))
    if(is.character(r)) {
      d$arq <- r
    }
    return(d)
  })
  if(is.character(r)) {
    d <- data.frame(arq = r, stringsAsFactors = FALSE)
  } else {
    d <- data.frame()
  }
  return(d)
}

pega_info <- function(kv, re) {
  x <- stringr::str_trim(stringr::str_match(rm_accent(kv), re)[,2])
  x
}

#' Funcao que faz o download das informacoes de um processo de primeiro
#' grau (PG) no TJSP.
#'
#' Retorna um data.frame com os metadados basicos e andamentos do processo
#'
#' @export
cpo_pg <- function(processos, path = NULL) {
  d <- dplyr::data_frame(n_processo = unique(processos))
  d <- dplyr::do(dplyr::group_by(d, n_processo),
                 cpo_pg_um(.$n_processo, path = path))
  d <- dplyr::ungroup(d)
  d
}

#' Funcao que faz o download das informacoes de um processo de primeiro
#' grau (PG) no TJSP.
#'
#' @export
cpo_pg_um <- function(p, path) {
  p <- gsub('[^0-9]', '', p)
  if(!is.null(path) & file.exists(sprintf('%s/%s.html', path, p))) {
    return(data.frame())
  }
  Sys.sleep(1)
  u <- build_url_cpo_pg(p)
  r <- httr::GET(u)
  k <- TRUE
  while (r$status_code != 200) {
    if (k) {
      cat("\nesperando...")
    }
    else {
      cat("...")
    }
    Sys.sleep(2)
    r <- httr::GET(u)
    k <- FALSE
  }
  if (!k) cat("\n")
  if(!is.null(path)) {
    cat(httr::content(r, 'text'), file = sprintf('%s/%s.html', path, p))
  }
  return(parse_cpo_pg_um(r))
}

build_url_cpo_pg <- function(p) {
  p <- gsub("[^0-9]", "", as.character(p))
  dados_url <- list("paginaConsulta" = "1",
                    "localPesquisa.cdLocal" = "-1",
                    "cbPesquisa" = "NUMPROC",
                    "tipoNuProcesso" = "UNIFICADO",
                    "numeroDigitoAnoUnificado" = "",
                    'foroNumeroUnificado' = "",
                    'dePesquisaNuUnificado' = "",
                    'dePesquisa' = "")
  dados_url[["numeriDigitoAnoUnificado"]] <- stringr::str_sub(p, end = 15)
  dados_url[["foroNumeroUnificado"]] <- stringr::str_sub(p, start = 22)
  dados_url[["dePesquisaNuUnificado"]] <- p
  url1 <- "http://esaj.tjsp.jus.br/cpo/pg/search.do"
  parametros <- paste(names(dados_url), unlist(dados_url), sep = "=")
  url2 <- paste(url1, paste0(parametros, collapse = "&"), sep = "?")
  url2
}

#' @export
parse_anda_um <- function(f) {
  try({
    h <- rvest::html(f, encoding = "UTF-8")
    tabela <- rvest::html_node(h, '#tabelaTodasMovimentacoes')
    d <- rvest::html_table(tabela)
    d <- dplyr::select(d, data = X1, anda = X3)
    d$anda <- gsub('[\t\r]', '', d$anda)
    d$anda <- gsub(' +', ' ', d$anda)
    d$anda <- gsub('(\n ?)+', '\n', d$anda)
    d$data <- as.Date(lubridate::dmy(d$data))
    d$arq <- f
    return(d)
  })
  if(is.character(f)) {
    d <- data.frame(arq = f, stringsAsFactors = FALSE)
  } else {
    d <- data.frame()
  }
  return(d)
}

#_______________________________________________________________________________

download_cpo_pg <- function(p, path = NULL, keep_html = TRUE) {

}

load_cpo_pg <- function(path, mov = TRUE, keyval = TRUE) {

}
