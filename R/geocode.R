#' Get GPS (lon, lat) from address
#'
#' @param addr Address string
#' @param source API source: \code{map.com}, \code{tgos}, or \code{mix}
#' @param parallel Parallel connection
#' @param n_cpu 
#' @param rate Connection rate per second
#' @param use_tor Boolean. Default TRUE. Use tor or not.
#'
#' @describeIn geocode Get GPS from address vector
#' @return GPS data.table
#' @export
#' @import magrittr data.table parallel pbapply
#'
#' @examples
#' addr <- c("台北市中正區羅斯福路一段２號",
#'           "台北市中正區貴陽街一段１２０號")
#' geocode(addr)
geocode <- function(addr, source = c("map.com", "tgos", "mix"), 
                    precise = FALSE, 
                    parallel = TRUE, n_cpu = -1L, 
                    rate = 200, use_tor = TRUE) {
  # addr <- c("台北市中正區羅斯福路一段２號",
  #           "台北市中正區貴陽街一段１２０號")
  if (!is.vector(addr)) {
    stop("values must be character vecter")
  }

  
  n_oginial_addr <- length(addr)
  addr <- unique(addr)
  message(sprintf("%d unique address out of %d input",
                  length(addr), n_oginial_addr))

  
  source <- match.arg(source)
  
  geocode_ <- switch (source,
    `map.com` = geocode_map_com_,
    `tgos` = function(...) geocode_tgos_(keystr = get_keystr(), ...)
  )
  
  if (n_cpu != 1 && length(addr) >= 10) {
    is_parallel <- TRUE
  } else {
    is_parallel <- FALSE
  }
  if (n_cpu == -1L) {n_cpu <- parallel::detectCores() - 1}
  
  if (use_tor) message("(Use tor in crawling)");

  if (is_parallel) {
    message(sprintf("(Use %s clusters)", n_cpu))
    
    cl <- parallel::makeCluster(n_cpu)
    print(cl)
    on.exit(parallel::stopCluster(cl))

    worker.init <- function(packages) {
      for (p in packages) {
        library(p, character.only=TRUE)
      }
      invisible(NULL)
    }
    invisible(
      parallel::clusterCall(cl, worker.init, c('magrittr', 'httr', 'rvest', 'data.table'))
    )

    out <- pbapply::pbsapply(addr, geocode_, precise = precise, 
                             rate = rate, use_tor = use_tor,
                             simplify = FALSE, USE.NAMES = TRUE,
                             cl = cl) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  } else {
    out <- pbapply::pbsapply(addr, geocode_, precise = precise, 
                             rate = rate, use_tor = use_tor,
                  simplify = FALSE, USE.NAMES = TRUE) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  }

  # Fetch 2nd time
  left <- out[is.na(lat), addr]
  if (length(left) > 0) {
    message(sprintf("Fetch 2nd time for left %d addr...", length(left)))
    
    if (length(left) > 500 && !use_tor) {
      use_tor <- TRUE
      message(sprintf("Use tor for left %d data", length(left)))
    }
    if (is_parallel) {
      message(sprintf("(Use %s clusters)", n_cpu))
      
      temp <- left %>%
        pbapply::pbsapply(geocode_, precise = precise, 
                          rate = rate, use_tor = use_tor,
                          simplify = FALSE, USE.NAMES = TRUE,
                          cl = cl) %>%
        rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
    } else {
      temp <- left %>%
        pbapply::pbsapply(., geocode_, precise = precise, 
                          rate = rate, use_tor = use_tor,
               simplify = FALSE, USE.NAMES = TRUE) %>%
        rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
    }
    out <- rbindlist(list(out[!is.na(lat),], temp), fill=TRUE, use.names = TRUE)
  }
  
  # # Fetch 3rd time (w/o tor)
  # left <- out[is.na(lat), addr]
  # if (length(left) > 500) {
  #   use_tor <- TRUE
  #   message(sprintf("Using tor for left %d data", length(left)))
  # } else {
  #   use_tor <- FALSE
  # }
  # if (is_parallel) {
  #   temp <- left %>%
  #     pbapply::pbsapply(geocode_, rate = rate, use_tor = use_tor,
  #                       simplify = FALSE, USE.NAMES = TRUE,
  #                       cl = cl) %>%
  #     rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  # } else {
  #   temp <- left %>%
  #     sapply(., FUN = geocode_, rate = rate, use_tor = use_tor,
  #            simplify = FALSE, USE.NAMES = TRUE) %>%
  #     rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  # }
  # out <- rbindlist(list(out[!is.na(lat),], temp), fill=TRUE, use.names = TRUE)

  out[]
}
