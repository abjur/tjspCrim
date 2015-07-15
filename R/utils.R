#' @export
rm_accent <- function (x) gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))

`%>%` <- dplyr::`%>%`

#' @export
foros_tjsp <- function(coma = NULL) {
  link <- 'http://wwwh.cnj.jus.br/portal/images/programas/numeracao-unica/tribunais-estaduais/foros-1.xls'
  a <- tempfile()
  foros <- download.file(link, destfile = a)
  d <- gdata::read.xls(a, encoding = 'latin1', stringsAsFactors = FALSE)
  names(d) <- c('codigo', 'foro_origem', 'comarca_origem')
  d <- dplyr::mutate(d, codigo = sprintf('%04d', as.numeric(codigo)))
  file.remove(a)
  if(!is.null(coma)) {
    d <- dplyr::filter(d, stringr::str_detect(comarca, coma))
  }
  d
}

#' @export
add_key <- function(v, re, lab) {
  x <- ifelse(str_detect(v, re), lab, '')
  x
}

