#' get genes from GO or Mapman terms
#'
#' @param terms GO or Mapman terms in form of an array
#' @param name task related to terms (go or mapman)
#'
#' @return vector of gene names related to the terms
#' @export
#'
#' @examples
term2gene <- function(terms = character(0), name = c("go", "mapman"), 
                      host="https://microasp.upsc.se", port= 5432,
                      url='pabies') {
  

  
  if (length(terms) == 1) {
    terms <- rep(terms, 2)
  }
  tryCatch({
    request <- httr::POST(paste0(host,":",port,"/",url,"/",'term-to-gene'),
                          body = list(
                            target = list(terms = terms, name = name)
                          ),
                          encode = "json")
    #print(request)
    parsed <- jsonlite::fromJSON(httr::content(request, as = "text",
                                               encoding = "UTF-8"))
    #print(parsed)
    result <- unlist(parsed[[name]]$ids)
    
    return(result)
  }, error=function(e) {
    return(paste("Error\n",e))
  })
}
