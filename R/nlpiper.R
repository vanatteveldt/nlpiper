#' POST a request and decode results
.post <- function(url, body, token=NULL) {
  if (getOption("nlpiper.verbose", default=F)) message("POST ", url)
  token = getOption("nlpiper.token")
  config = if (!is.null(token)) httr::add_headers(Authorization=paste("Token", token)) else list()
  res = httr::POST(url, config, body=body, httr::content_type_json())
  content = httr::content(res, as="text", encoding = "utf-8")


  httr::stop_for_status(res, paste("POST", url, ":", content))
  jsonlite::fromJSON(content)
}

#' Process a text with NLPipe
#'
#' For instructions for setting up an NLPipe server, see \url{https://github.com/vanatteveldt/nlpipe}
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param texts Texts to process
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param ids optional explicit ids
#' @param as_ascii if TRUE, force convert text to ascii before processing
#'
#' @return IDs of the tasks
#' @examples
#' \dontrun{
#' ## setup corenlp, e.g. run corenlp docker.
#' ## For instructions see  <https://github.com/vanatteveldt/nlpipe>
#'
#' ## start process
#' task_id = nlpiper::process_async("corenlp_lemmatize", c("This is an example.", "This as well."))
#'
#' ## check status
#' nlpiper::status("corenlp_lemmatize", ids = task_id)
#'
#' ## retrieve results
#' nlpiper::result('corenlp_lemmatize', ids = task_id, format = 'csv')
#' }
#' @export
process_async <- function(module, texts, server=getOption("nlpiper.server", default="http://localhost:5001"), ids=NULL, as_ascii=F) {
  url = sprintf("%s/api/modules/%s/bulk/process", server, module)
  if (as_ascii) texts = iconv(texts, to='ASCII//TRANSLIT')
  body = if (is.null(ids)) jsonlite::toJSON(texts) else jsonlite::toJSON(stats::setNames(as.list(texts), ids), auto_unbox = T)
  .post(url, body)
}

#' Check NLPipe processing status
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param ids IDs of the task to check status
#' @param server NLPipe server or local folder (default: localhost:5001)
#'
#' @return A string for each task indicating processing status
#' @examples
#' \dontrun{
#' ## setup corenlp, e.g. run corenlp docker.
#' ## For instructions see  <https://github.com/vanatteveldt/nlpipe>
#'
#' ## start process
#' task_id = nlpiper::process_async("corenlp_lemmatize", c("This is an example.", "This as well."))
#'
#' ## check status
#' nlpiper::status("corenlp_lemmatize", ids = task_id)
#'
#' ## retrieve results
#' nlpiper::result('corenlp_lemmatize', ids = task_id, format = 'csv')
#' }
#' @export
status <- function(module, ids, server=getOption("nlpiper.server", default="http://localhost:5001")) {
  url = sprintf("%s/api/modules/%s/bulk/status", server, module)
  body = jsonlite::toJSON(ids)
  status = .post(url, body)
  sapply(ids, function(x) status[[as.character(x)]])
}

#' Fetch NLPipe results
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param ids IDs of the task to get results for
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param format The format to download results as (e.g. csv)
#' @param check_status If TRUE (default), first check status and only retrieve articles that are DONE
#'
#' @return The processed text(s), or NA if status was not 'DONE'. If format is csv, return a single data frame with all result lines
#' @examples
#' \dontrun{
#' ## setup corenlp, e.g. run corenlp docker.
#' ## For instructions see  <https://github.com/vanatteveldt/nlpipe>
#'
#' ## start process
#' task_id = nlpiper::process_async("corenlp_lemmatize", c("This is an example.", "This as well."))
#'
#' ## check status
#' nlpiper::status("corenlp_lemmatize", ids = task_id)
#'
#' ## retrieve results
#' nlpiper::result('corenlp_lemmatize', ids = task_id, format = 'csv')
#' }
#' @export
result <- function(module, ids, server=getOption("nlpiper.server", default="http://localhost:5001"), format=NULL, check_status=TRUE) {
  if (check_status) {
    message("Checking status of ", length(ids), " results")
    status = status(module, ids, server=server)
    not_done = length(status[status != "DONE"])
    if (not_done > 0) warning("Skipping ", not_done, "/",length(ids)," unfinished documents")
    ids = ids[status == "DONE"]
  }
  url = sprintf("%s/api/modules/%s/bulk/result", server, module)
  if (!is.null(format)) url=sprintf("%s?format=%s", url, format)
  message("Retrieving ", length(ids), " results from ", url)

  chunks = split(ids, ceiling(seq_along(ids)/1000))
  result = list()
  for(i in seq_along(chunks)) {
    chunk = chunks[[i]]
    message("  [",i,"/",length(chunks),"] Retrieving ", length(chunk), " results")
    body = jsonlite::toJSON(chunk)
    chunk_result = .post(url, body)
    result = c(result, chunk_result)
  }

  if (!is.null(format) && format=="csv") {
    message("Converting results into data table")
    dfs = vector('list', length(result))
    for(i in seq_along(result))
      if (!is.na(result[[i]])) {
        con = textConnection(result[[i]])
        df = utils::read.csv(con)
        close(con)
        dfs[[i]] = df
      }
    data.table::rbindlist(dfs)
  } else {
    sapply(ids, function(x) result[[as.character(x)]])
  }
}

#' Process a text with NLPipe and wait for result
#'
#' For instructions for setting up an NLPipe server, see \url{https://github.com/vanatteveldt/nlpipe}
#'
#' @param module Name of the NLPipe module to call (e.g. test_upper, corenlp_lemmatize)
#' @param text Text to process
#' @param server NLPipe server or local folder (default: localhost:5001)
#' @param format The format to download results as (e.g. csv)
#' @param ids optinal explicit ids
#' @param as_ascii if TRUE, force convert text to ascii before processing
#'
#' @return The processed text
#' @examples
#' \dontrun{
#' ## setup corenlp, e.g. run corenlp docker.
#' ## For instructions see  <https://github.com/vanatteveldt/nlpipe>
#'
#' nlpiper::process("test_upper", "test")
#' nlpiper::process("corenlp_lemmatize", "This is an example.", format = 'csv')
#' }
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
