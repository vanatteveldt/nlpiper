#' Process a text with NLPipe
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param text Text to process
#' @param server NLPipe server or local folder (default: localhost:5001)
#'
#' @return The ID of the task
#' @export
process_async <- function(module, text, server=getOption("nlpiper.server", default="http://localhost:5001")) {
  url = paste(server, "/api/modules/", module, "/", sep = "")
  message("POST ", url)
  res = httr::POST(url, body=text)
  if (res$status_code != 202) stop("Error on POST ", url,":", res$content)
  return(res$headers$id)
}

#' Check NLPipe processing status
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param id ID of the task to check status
#' @param server NLPipe server or local folder (default: localhost:5001)
#'
#' @return A string indicating processing status
#' @export
status <- function(module, id, server=getOption("nlpiper.server", default="http://localhost:5001")) {
  url = paste(server, "/api/modules/", module, "/", id, sep = "")
  message("HEAD ", url)
  res = httr::HEAD(url)
  if (!"status" %in% names(res$headers)) stop("Error on HEAD ", url, ":", res$status_code, " ", res$content)
  return(res$headers$status)
}

#' Fetch NLPipe results
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param id ID of the task to get results for
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param format The format to download results as (e.g. csv)
#'
#' @return The processed text
#' @export
result <- function(module, id, server=getOption("nlpiper.server", default="http://localhost:5001"), format=NULL) {
  url = paste(server, "/api/modules/", module, "/", id, sep = "")
  if (!is.null(format)) url = paste(url, "?format=", format, sep = "")
  message("GET ", url)
  res = httr::GET(url)
  if (res$status_code != 200) stop("Error on GET ", url, ":", res$status_code, " ", res$content)
  result = httr::content(res, "text")
  if (!is.null(format) && format=="csv") {
    con = textConnection(result)
    result = read.csv(con)
    close(con)
  }
  result
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
