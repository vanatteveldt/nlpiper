#' Process a text with NLPipe
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param texts Texts to process
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param ids optional explicit ids
#'
#' @return IDs of the tasks
#' @export
process_async <- function(module, texts, server=getOption("nlpiper.server", default="http://localhost:5001"), ids=NULL, as_ascii=F) {
  url = sprintf("%s/api/modules/%s/bulk/process", server, module)
  if (getOption("nlpiper.verbose", default=F)) message("POST ", url)
  if (as_ascii) texts = iconv(texts, to='ASCII//TRANSLIT')
  body = if (is.null(ids)) jsonlite::toJSON(texts) else jsonlite::toJSON(setNames(as.list(texts), ids), auto_unbox = T)
  res = httr::POST(url, body=body, httr::content_type_json())
  if (floor(res$status_code/100) != 2) stop("Error on POST ", url,":", res$content)
  jsonlite::fromJSON(httr::content(res, as="text"))
}

#' Check NLPipe processing status
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param ids IDs of the task to check status
#' @param server NLPipe server or local folder (default: localhost:5001)
#'
#' @return A string for each task indicating processing status
#' @export
status <- function(module, ids, server=getOption("nlpiper.server", default="http://localhost:5001")) {
  url = sprintf("%s/api/modules/%s/bulk/status", server, module)
  if (getOption("nlpiper.verbose", default=F)) message("POST ", url)
  body = jsonlite::toJSON(ids)
  res = httr::POST(url, body=body, httr::content_type_json())
  if (floor(res$status_code/100) != 2) stop("Error on POST ", url,":", res$content)
  status = jsonlite::fromJSON(httr::content(res, as="text"))
  sapply(ids, function(x) status[[as.character(x)]])
}

#' Fetch NLPipe results
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param id IDs of the task to get results for
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param format The format to download results as (e.g. csv)
#'
#' @return The processed text(s), or NA if status was not 'DONE'. If format is csv, return a single data frame with all result lines
#' @export
result <- function(module, ids, server=getOption("nlpiper.server", default="http://localhost:5001"), format=NULL) {
  url = sprintf("%s/api/modules/%s/bulk/result", server, module)
  if (!is.null(format)) url=sprintf("%s?format=%s", url, format)
  if (getOption("nlpiper.verbose", default=F)) message("POST ", url)
  body = jsonlite::toJSON(ids)
  res = httr::POST(url, body=body, httr::content_type_json())
  if (floor(res$status_code/100) != 2) stop("Error on POST ", url,":", res$content)
  results = jsonlite::fromJSON(httr::content(res, as="text", encoding = "utf-8"))
  results = sapply(ids, function(x) results[[as.character(x)]])
  # convert csv objects to single df
  if (!is.null(format) && format=="csv") {
    dfs = list()
    for(i in seq_along(results)) if (!is.na(results[i])) {
      con = textConnection(results[i])
      result = read.csv(con)
      close(con)
      dfs = c(dfs,list(result))
    }
    results = plyr::rbind.fill(dfs)
  }
  return(results)
}

#' Process a text with NLPipe and wait for result
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param text Text to process
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param format The format to download results as (e.g. csv)
#' @param ids optinal explicit ids
#'
#' @return The processed text
#' @export
process <- function(module, text, server=getOption("nlpiper.server", default="http://localhost:5001"), format=NULL, ids=NULL, as_ascii=F) {
  id = process_async(module, text, server, ids=ids, as_ascii=as_ascii)
  while(T) {
    status = status(module, id, server)
    if (status == "DONE") {
      return(result(module, id, server, format=format))
    }
    Sys.sleep(0.5)
  }
}
