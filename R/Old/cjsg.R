#' Funcao que faz o download da lista de processos na Consulta de Julgados do Segundo Grau (CJSG)
#' do TJSP.
#'
#' @export
cjsg <- function(livre = NULL,
                 ementa = NULL,
                 relator = NULL,
                 prolator = NULL,
                 classes = NULL,
                 assuntos = NULL,
                 comarcas = NULL,
                 orgaos = NULL,
                 data_inicial = NULL, data_final = NULL,
                 origem = c('2grau', 'colegio_recursal'),
                 tipo = c('acordao', 'acordo', 'monocratica'),
                 min_pag = 1,
                 max_pag = 10,
                 salvar = FALSE,
                 path = '') {

  dados <- dados_cjsg(livre, ementa, relator, prolator, classes, assuntos,
                      comarcas, orgaos, data_inicial, data_final, origem, tipo)
  r <- httr::POST('https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do',
                  body = dados,
                  config = list(ssl.verifypeer = FALSE))

  if(is.infinite(max_pag)) {
    h <- XML::htmlParse(httr::content(r, 'text'), encoding = 'UTF-8')
    val <- XML::xmlGetAttr(XML::getNodeSet(h, "//*[@id='totalResultadoAba-A']")[[1]], 'value')
    num <- as.numeric(val)
    max_pag <- ceiling(num / 10)
    cat(sprintf('Paginas indo de %d a %d (total: %d)',
                min_pag, max_pag, max_pag - min_pag + 1), '\n')
  }
  d <- dplyr::bind_rows(lapply((min_pag):(max_pag),
                               crawler_cjsg_pag,
                               r = r, salvar = salvar, path = path))
  return(d)
}

#' @export
dados_cjsg <- function(livre, ementa, relator, prolator, classes, assuntos,
                       comarcas, orgaos, data_inicial, data_final, origem, tipo) {
  d <- list('dados.buscaInteiroTeor'='',
            'dados.pesquisarComSinonimos'='S',
            'dados.pesquisarComSinonimos'='S',
            'dados.buscaEmenta'='',
            'dados.nuProcOrigem'='',
            'dados.nuRegistro'='',
            'agenteSelectedEntitiesList'='',
            'contadoragente'='0',
            'contadorMaioragente'='0',
            'codigoCr'='',
            'codigoTr'='',
            'nmAgente'='',
            'juizProlatorSelectedEntitiesList'='',
            'contadorjuizProlator'='0',
            'contadorMaiorjuizProlator'='0',
            'codigoJuizCr'='',
            'codigoJuizTr'='',
            'nmJuiz'='',
            'classesTreeSelection.values'='',
            'classesTreeSelection.text'='',
            'assuntosTreeSelection.values'='',
            'assuntosTreeSelection.text'='',
            'comarcaSelectedEntitiesList'='',
            'contadorcomarca'='0',
            'contadorMaiorcomarca'='0',
            'cdComarca'='',
            'nmComarca'='',
            'secoesTreeSelection.values'='0-435,0-436,0-433,0-81,0-103,0-110,0-947,0-802,0-80,0-440,0-94,0-117,0-123,0-967,0-1006,0-1164,0-96,0-108,0-118,0-694,0-968,0-1165,0-97,0-119,0-703,0-1166,0-99,0-109,0-120,0-704,0-445,0-446,0-100,0-121,0-695,0-447,0-688,0-450,0-451,0-971,0-452,0-453,0-454,0-800,0-455,0-801,0-456,0-457,0-459,0-460,0-1188,0-82,0-705,0-692,0-948,0-803,0-83,0-462,0-463,0-465,0-466,0-1189,0-84,0-104,0-706,0-699,0-949,0-86,0-468,0-470,0-471,0-1192,0-85,0-111,0-122,0-961,0-89,0-472,0-474,0-475,0-1228,0-87,0-632,0-112,0-700,0-962,0-92,0-476,0-478,0-479,0-1008,0-1229,0-88,0-113,0-696,0-963,0-994,0-95,0-480,0-482,0-483,0-90,0-105,0-114,0-701,0-964,0-926,0-98,0-484,0-485,0-91,0-106,0-115,0-693,0-965,0-972,0-486,0-93,0-107,0-116,0-702,0-966,0-1140,0-642,0-643,0-646,0-647,0-644,0-645,0-904,0-224,0-641,0-640,0-0,0-911,0-913,0-633,0-636,0-637,0-634,0-945,0-639,0-638,0-635,0-1076,0-912',
            'secoesTreeSelection.text'='146 Registros selecionados',
            'dados.dtJulgamentoInicio'='01/01/2014',
            'dados.dtJulgamentoFim'='31/12/2014',
            'dados.dtRegistroInicio'='',
            'dados.dtRegistroFim'='',
            'dados.origensSelecionadas'='T',
            'tipoDecisaoSelecionados'='A',
            'tipoDecisaoSelecionados'='H',
            'tipoDecisaoSelecionados'='D',
            'dados.ordenacao'='data')
  d
}


#' Calcula quantas paginas tem uma query no CJPG
#'
#' @export
n_cjsg <- function(livre = NULL,
                   ementa = NULL,
                   relator = NULL,
                   prolator = NULL,
                   classes = NULL,
                   assuntos = NULL,
                   comarcas = NULL,
                   orgaos = NULL,
                   data_inicial = NULL, data_final = NULL,
                   origem = c('2grau', 'colegio_recursal'),
                   tipo = c('acordao', 'acordo', 'monocratica')) {

  dados <- dados_cjsg(livre, ementa, relator, prolator, classes, assuntos,
                      comarcas, orgaos, data_inicial, data_final, origem, tipo)
  r <- httr::POST('https://esaj.tjsp.jus.br/cjsg/resultadoCompleta.do',
                  body = dados,
                  config = list(ssl.verifypeer = FALSE))

  h <- XML::htmlParse(httr::content(r, 'text'), encoding = 'UTF-8')
  val <- XML::xmlGetAttr(XML::getNodeSet(h, "//*[@id='totalResultadoAba-A']")[[1]], 'value')
  num <- as.numeric(val)
  return(num)
}

#' @export
parse_node_meta_cjsg <- function(node) {
  val <- XML::xmlValue(node)
  val <- stringr::str_trim(stringr::str_split_fixed(gsub('[\n\r\t]','', val), ':', 2))
  df <- data.frame(val[2], stringsAsFactors = FALSE)
  names(df) <- val[1]
  df
}

#' @export
parse_node_cjsg <- function(node, salvar = FALSE, path = '', pag) {
  children <- XML::xmlChildren(node)
  df <- do.call(cbind, lapply(children[2:(length(children))], parse_node_meta_cjsg))
  a <- XML::xmlChildren(XML::xmlChildren(children[[1]])$td)$a
  df$n_processo <- gsub('[\n\r\t ]', '', XML::xmlValue(a))
  df$cdacordao <- XML::xmlGetAttr(a, 'cdacordao')
  df$cdforo <- XML::xmlGetAttr(a, 'cdforo')
  classe_assunto <- XML::getNodeSet(children[[1]], './/span[@class="assuntoClasse"]')[[1]]
  classe_assunto <- XML::xmlValue(classe_assunto)
  classe_assunto <- stringr::str_trim(gsub('[\n\r\t]','', classe_assunto))
  df$classe_assunto <- classe_assunto
  txt_format <- XML::getNodeSet(node, './/textarea[contains(@id, "textAreaDados")]')[[1]]
  txt_format <- XML::xmlValue(txt_format)
  df$txt_format <- txt_format
  df$pag <- pag
  if(salvar) {
    n_processo_num <- gsub('[^0-9]', '', df$n_processo)
    arq <- sprintf('%s/%06d_%s_%s.rds', path, pag, n_processo_num, df$cdacordao)
    if(!file.exists(arq)) {
      saveRDS(df, file = arq)
    }
  }
  df
}

crawler_cjsg_pag <- function(pag, r, salvar = FALSE, path = '') {
  # Sys.sleep(1)
  url_pag <- sprintf('https://esaj.tjsp.jus.br/cjsg/trocaDePagina.do?tipoDeDecisao=A&pagina=%d', pag)
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
    df <- dplyr::bind_rows(lapply(nodes, parse_node_cjsg, salvar = salvar,
                                  path = path, pag = pag))
    df$pag <- pag
    names(df) <- gsub('\\(a\\)', '', gsub(' +', '_', tolower(rm_accent(names(df)))))
    cat('OK!\n')
    return(df)
  }, TRUE)
  cat('ERROR !!!\n')
  return(data.frame())
}
