#' Process a text with NLPipe
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param texts Texts to process
#' @param server NLPipe server or local folder (default: localhost:5001)
#'
#' @return IDs of the tasks
#' @export
process_async <- function(module, texts, server=getOption("nlpiper.server", default="http://localhost:5001")) {
  url = paste(server, "/api/modules/", module, "/", sep = "")
  message("POST ", url)
  result = numeric(length = length(texts))
  for (i in seq_along(texts)) {
    res = httr::POST(url, body=texts[i])
    if (res$status_code != 202) stop("Error on POST ", url,":", res$content)
    result[i] = res$headers$id
  }
  return(result)
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
  result = character(length = length(ids))
  for (i in seq_along(ids)) {
    url = paste(server, "/api/modules/", module, "/", ids[i], sep = "")
    message("HEAD ", url)
    res = httr::HEAD(url)
    if (!"status" %in% names(res$headers)) stop("Error on HEAD ", url, ":", res$status_code, " ", res$content)
    result[i] = res$headers$status
  }
  return(result)
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
  results = character(length=length(ids))
  for (i in seq_along(ids)) {
    url = paste(server, "/api/modules/", module, "/", id, sep = "")
    if (!is.null(format)) url = paste(url, "?format=", format, sep = "")
    message("GET ", url)
    res = httr::GET(url)
    if (res$status_code != 200) {
      warning("Error on GET ", url, ":", res$status_code, " ", res$content)
      results[i] = NA
    } else {
      results[i] = httr::content(res, "text")
    }
  }

  # convert csv objects to single df
  if (!is.null(format) && format=="csv") {
    dfs = list()
    for(i in seq_along(results)) if (!is.na(results[i])) {
      con = textConnection(results[i])
      result = read.csv(con)
      close(con)
      result$`.id` = ids[i]
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
#'
#' @return The processed text
#' @export
process <- function(module, text, server=getOption("nlpiper.server", default="http://localhost:5001"), format=NULL) {
  id = process_async(module, text, server)
  while(T) {
    status = status(module, id, server)
    if (status == "DONE") {
      return(result(module, id, server, format=format))
    }
    Sys.sleep(0.5)
  }
}
