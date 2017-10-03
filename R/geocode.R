library(parallel)
library(httr)
library(rvest)
library(data.table)
library(pbapply)

#' Get GPS (lon, lat) from address
#'
#' @param addr Address string
#' @param source API source
#' @param parallel Parallel connection
#' @param n_cpu 
#' @param rate Connection rate per second
#' @param use_tor Boolean. Default TRUE. Use tor or not.
#'
#' @describeIn geocode Get GPS from address vector
#' @return GPS data.table
#' @export
#' @import magrittr httr rvest data.table parallel
#'
#' @examples
#' addr <- c("台北市中正區羅斯福路一段２號",
#'            "台北市中正區貴陽街一段１２０號")
#' geocode(addr)
geocode <- function(addr, precise = FALSE, source = "google", 
                    parallel = TRUE, n_cpu = -1L, 
                    rate = 200, use_tor = TRUE) {
  # addr <- c("台北市中正區羅斯福路一段２號",
  #            "台北市中正區貴陽街一段１２０號")
  if (!is.vector(addr)) {
    stop("values must be character vecter")
  }

  n_oginial_addr <- length(addr)
  addr <- unique(addr)
  message(sprintf("%d unique address out of %d input",
                  length(addr), n_oginial_addr))

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

    out <- pbapply::pbsapply(addr, geocode_, rate = rate, use_tor = use_tor,
                             simplify = FALSE, USE.NAMES = TRUE,
                             cl = cl) %>%
      rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
  } else {
    out <- pbapply::pbsapply(addr, geocode_, rate, use_tor = use_tor,
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
        pbapply::pbsapply(geocode_, rate = rate, use_tor = use_tor,
                          simplify = FALSE, USE.NAMES = TRUE,
                          cl = cl) %>%
        rbindlist(idcol = "addr", fill=TRUE, use.names = TRUE)
    } else {
      temp <- left %>%
        pbapply::pbsapply(., geocode_, rate = rate, use_tor = use_tor,
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

  if (precise) {
    to_clean <- setdiff(names(out),  "addr")
    out[village == "", (to_clean) := NULL]
  }
  
  out[]
}

geocode_ <- function(addr, rate=200, use_tor = FALSE, ...) {
  # addr <- "台北市中正區羅斯福路一段２號"

  ## to be nice :)
  Sys.sleep(rexp(1, rate)) # sleep expo dist at rate per sec

  url <- "http://api.map.com.tw/net/GraphicsXY.aspx"
  get_ <- function(use_tor, max_try = 3) {

    i <- 1
    while (i <= max_try) {
      if (use_tor) {
        set_config(use_proxy("socks5://localhost:9050"))
        # set_config(verbose())
      }
      
      err <- tryCatch({
        res <- GET(url,
                   add_headers(
                     Referer = "http://www.map.com.tw/",
                     `User-Agent` = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36"
                   ),
                   query = list(
                     search_class = "address",
                     SearchWord = addr %>% clean_addr,
                     searchkey = "D43A19151569F32A449B7EDCB8555165B68B5F95"))
        break
      }, error = function(e) {
        i <<- i + 1
        if (i == max_try) {
          if (e$message == "Couldn't connect to server" && use_tor) {
            warning("TOR connection may be faild. Restart TOR with `sudo killall tor; tor &`", call. = FALSE)
            stop(e)
          }
          stop(e)
        }
        res <<- NULL
        invisible(e)
      })
      reset_config()
    }

    reset_config()
    res
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
