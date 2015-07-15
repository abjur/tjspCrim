#' PDF to text
#'
#' @export
pdf2txt <- function(a) {
  link <- 'http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?dtDiario=01/04/2015&cdCaderno=12'
  download.file(link)

}

#' @export
download_arq <- function(link, a) {
  passa <- FALSE
  if(file.exists(a)) {
    cat('arquivo ',  a, ' ja existe!\n')
    return('sucesso')
  }
  cat('baixando ', a, '...', sep = '')
  x <- download.file(link, destfile = a, quiet = TRUE)
  if(x == 0) {
    cat('OK!\n')
    return('sucesso')
  }
  cat('ERRO!\n')
  return('erro')
}

#' PDF to text
#'
#' @export
download_doe <- function(tipo = 'all', datas, path = NULL) {
  if(tipo == 'central') {
    date_link <- format(as.Date(datas), '%d/%m/%Y')
    link <- sprintf('http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?dtDiario=%s&cdCaderno=12', date_link)
    a <- sprintf('%s/%s_%s.pdf', path, tipo, datas)
    aux <- sapply(1:length(a), function(i) {
      download.file(link[i], destfile = a[i])
    })
    return(a)
  }
  if(tipo == 'all') {
    fixo <- 'http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?'
    d <- expand.grid(datas = datas, caderno = as.character(c(11:15, 18)))
    d <- dplyr::mutate(d,
                       date_link = format(as.Date(datas), '%d/%m/%Y'),
                       link = sprintf('%sdtDiario=%s&cdCaderno=%s', fixo, date_link, caderno),
                       arq = sprintf('%s/%s_%s.pdf', path, caderno, datas))
    d <- data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
    d <- dplyr::arrange(d, desc(datas))
    d <- dplyr::rowwise(d)
    d <- dplyr::mutate(d, baixou = download_arq(link, arq))
    d <- dplyr::ungroup(d)
    return(d)
  }

}

#' Parser DOE
#'
#' @export
load_doe_um <- function(a) {
  try({
    test <- readr::read_lines(a, 1)
    if(stringi::stri_detect(test, fixed = 'PDF')) {
      arq <- gsub('\\.pdf$', '.rds', a)
      if(!file.exists(arq)) {
        system(sprintf('pdftotext %s', a))
        a <- gsub('\\.pdf$', '.txt', a)
        f <- readr::read_file(a)
        f <- stringi::stri_replace_all(f, regex = '[\n\f\r]', replacement = ' ')
        r <- stringi::stri_extract_all(f, regex = '[0-9]{7}\\-[0-9]{2}\\.[0-9]{4}\\.8\\.26\\.[0-9]{4}')[[1]]
        file.remove(a)
        saveRDS(r, file = arq)
        return(r)
      }
    } else {
      return(list('erro'))
    }
  })
}

#' @export
load_doe <- function(a) {
  d <- dplyr::data_frame(arq = a)
  d <- dplyr::group_by(d, arq)
  d <- dplyr::do(d, n_processo = load_doe_um(.$arq))
  d <- dplyr::ungroup(d)
  d <- tidyr::unnest(d, n_processo)
  d <- dplyr::tbl_df(d)
  d
}

#' @export
baixa_tudo <- function(p) {
  datas <- as.character(ymd('2007-10-01') + days(0:1e5))
  a <- list.files(p)
  a2 <- as.character(ymd(stringr::str_extract(a, '[0-9]{4}-[0-9]{2}-[0-9]{1,2}')))
  datas <- datas[!datas %in% a2 & datas < '2015-04-01']
  download_doe(datas = datas, path = p)
}

