library(snowfall)
library(httr)
library(rvest)
library(data.table)

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
get_gps <- function(addrs, n_cpu = -1L, rate = 200) {
  # addrs <- c("台北市中正區羅斯福路一段２號",
  #            "台北市中正區貴陽街一段１２０號")
  if (!is.vector(addrs)) {
    stop("values must be character vecter")
  }

  addrs <- unique(addrs)

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
      NULL
    }
    clusterCall(cl, worker.init, c('httr', 'rvest', 'data.table'))

    # suppressMessages({
    #   snowfall::sfLibrary(httr, verbose = FALSE)
    #   snowfall::sfLibrary(rvest, verbose = FALSE)
    #   snowfall::sfLibrary(snowfall, verbose = FALSE)
    #   snowfall::sfLibrary(data.table, verbose = FALSE)
    # })

    out <- parallel::parSapplyLB(addrs, FUN = get_gps_, rate,
                                 simplify = FALSE, USE.NAMES = TRUE,
                                 cl = cl) %>%
      rbindlist(idcol = "addr")
  } else {
    out <- sapply(addrs, FUN = get_gps_, rate,
                  simplify = FALSE, USE.NAMES = TRUE) %>%
      rbindlist(idcol = "addr")
  }

  # Fetch 2nd time
  temp <- out[is.na(lat), addr] %>%
    sapply(., FUN = get_gps_, rate,
           simplify = FALSE, USE.NAMES = TRUE) %>%
  rbindlist(idcol = "addr")
  out <- rbindlist(list(out[!is.na(lat),], temp))
  out
}


#' @describeIn get_gps Get GPS from address vector length of one
get_gps_ <- function(addr, rate=200) {
  # addr <- "台北市中正區羅斯福路一段２號"

  ## to be nice :)
  Sys.sleep(rexp(1, rate)) # sleep expo dist at rate per sec

  url <- "http://api.map.com.tw/net/GraphicsXY.aspx"
  res <- GET(url,
             use_proxy("socks5://localhost:9050"), # tor proxy
             add_headers(
               Referer = "http://www.map.com.tw/",
               `User-Agent` = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36"
             ),
             query = list(
               search_class = "address",
               SearchWord = addr,
               searchkey = "D43A19151569F32A449B7EDCB8555165B68B5F95"))
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
  res <- res %>%
    content("text") %>%
    stringr::str_replace_all('^\\(|\\)$', '') %>%
    jsonlite::fromJSON() %>%
    .[[1]] %>%
    as.data.table
  res[!is.na(lat),
      addr_norm := paste0(baddr2, bname2, village, road)]
  res
}
