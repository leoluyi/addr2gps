library(parallel)
library(httr)
library(rvest)
library(data.table)
library(pbapply)

#' Get GPS (lon, lat) from address
#'
#' @param addr Address string
#' @param rate Connection rate per second
#'
#' @describeIn get_gps Get GPS from address vector
#' @return GPS data.table
#' @export
#' @import httr rvest data.table parallel
#'
#' @examples
#' addrs <- c("台北市中正區羅斯福路一段２號",
#'            "台北市中正區貴陽街一段１２０號")
#' get_gps(addrs)
get_gps <- function(addrs, n_cpu = -1L, rate = 200, use_tor = TRUE) {
  # addrs <- c("台北市中正區羅斯福路一段２號",
  #            "台北市中正區貴陽街一段１２０號")
  if (!is.vector(addrs)) {
    stop("values must be character vecter")
  }

  n_oginial_addr <- length(addrs)
  addrs <- unique(addrs)
  message(sprintf("Input %d address; processing %s unique",
                  n_oginial_addr, length(addrs)))

  if (n_cpu == -1L) {
    n_cpu <- parallel::detectCores() - 1
  }

  if (n_cpu > 1 && length(addrs) >= 10) {
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
      parallel::clusterCall(cl, worker.init, c('httr', 'rvest', 'data.table'))
    )

    if (use_tor) message("(Using tor in crawling)");
    out <- pbapply::pbsapply(addrs, get_gps_, rate = rate, use_tor = use_tor,
                             simplify = FALSE, USE.NAMES = TRUE,
                             cl = cl) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  } else {
    out <- sapply(addrs, FUN = get_gps_, rate, use_tor = FALSE,
                  simplify = FALSE, USE.NAMES = TRUE) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  }

  # Fetch 2nd time
  left <- out[is.na(lat), addr]
  message(sprintf("Fetch 2nd time for left %d addrs...", length(left)))
  if (length(left) > 500 && !use_tor) {
    use_tor <- TRUE
    message("(Using tor in crawling)")
  }
  if (n_cpu > 1 && length(left) >= 10) {
    temp <- left %>%
      pbapply::pbsapply(get_gps_, rate = rate, use_tor = use_tor,
                        simplify = FALSE, USE.NAMES = TRUE,
                        cl = cl) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  } else {
    temp <- left %>%
      sapply(., FUN = get_gps_, rate = rate, use_tor = use_tor,
             simplify = FALSE, USE.NAMES = TRUE) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  }
  out <- rbindlist(list(out[!is.na(lat),], temp), fill=TRUE, use.names = TRUE)

  # # Fetch 3rd time (w/o tor)
  # left <- out[is.na(lat), addr]
  # if (length(left) > 1000) {
  #   use_tor <- TRUE
  #   message(sprintf("Using tor for left %d data", length(left)))
  # } else {
  #   use_tor <- FALSE
  # }
  # temp <- left %>%
  #   sapply(., FUN = get_gps_, rate = rate, use_tor = use_tor,
  #          simplify = FALSE, USE.NAMES = TRUE) %>%
  #   rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  # out <- rbindlist(list(out[!is.na(lat),], temp), fill=TRUE, use.names = TRUE)

  out
}


#' @describeIn get_gps Get GPS from address vector length of one
get_gps_ <- function(addr, rate=200, use_tor = FALSE, ...) {
  # addr <- "台北市中正區羅斯福路一段２號"

  ## to be nice :)
  Sys.sleep(rexp(1, rate)) # sleep expo dist at rate per sec

  url <- "http://api.map.com.tw/net/GraphicsXY.aspx"
  get_ <- function(use_tor) {
    if (use_tor) {
      GET(url,
          use_proxy("socks5://localhost:9050"), # tor proxy
          add_headers(
            Referer = "http://www.map.com.tw/",
            `User-Agent` = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36"
          ),
          query = list(
            search_class = "address",
            SearchWord = addr,
            searchkey = "D43A19151569F32A449B7EDCB8555165B68B5F95"))
    } else {
      GET(url,
          add_headers(
            Referer = "http://www.map.com.tw/",
            `User-Agent` = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36"
          ),
          query = list(
            search_class = "address",
            SearchWord = addr,
            searchkey = "D43A19151569F32A449B7EDCB8555165B68B5F95"))
    }
  }

  res <- get_(use_tor)
  if (http_error(res)) {
    return(
      data.table(
        lat = NA_character_,
        lng = NA_character_,
        no = NA_character_,
        bheigh2 = NA_character_,
        bcount2 = NA_character_,
        baddr2 = NA_character_,
        bname2 = NA_character_,
        village = NA_character_,
        road = NA_character_,
        msg = NA_character_,
        precision = NA_character_
      )
    )
  }
  out <- res %>%
    content("text") %>%
    stringr::str_replace_all('^\\(|\\)$', '') %>%
    jsonlite::fromJSON() %>%
    .[[1]] %>%
    as.data.table
  out[!is.na(lat),
      addr_norm := paste0(baddr2, bname2, village, road)]
  out
}
