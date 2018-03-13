#' @import magrittr httr rvest stringr data.table
geocode_map_com_ <- function(addr, precise = FALSE, rate = 200, use_tor = FALSE, ...) {
  # addr <- "台北市中正區羅斯福路一段２號"
  
  ## to be nice :)
  Sys.sleep(rexp(1, rate)) # sleep expo dist at rate per sec
  
  url <- "http://api.map.com.tw/net/GraphicsXY.aspx"
  
  get_ <- function(addr, use_tor, max_try = 3) {
    
    if (use_tor) {
      old <- set_config(use_proxy("socks5://localhost:9050"))
      on.exit({
        set_config(old, override = TRUE)
      })
      # set_config(verbose())
    }
    
    i <- 1
    while (i <= max_try) {
      
      err <- tryCatch({
        res <- GET(url,
                   add_headers(
                     Referer = "http://www.map.com.tw/",
                     `User-Agent` = "Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.106 Safari/537.36"
                   ),
                   query = list(
                     search_class = "address",
                     SearchWord = addr,
                     searchkey = "D43A19151569F32A449B7EDCB8555165B68B5F95"))
        break
      }, error = function(e) {
        i <<- i + 1
        
        if (i > max_try) {
          stop(e)
        }
        
        if (e$message %>% str_detect("Couldn't connect to server|Failed to receive SOCKS5 connect request ack") &&
            use_tor) {
          warning("TOR connection may be faild. Restart TOR with `sudo killall tor; tor &`", call. = FALSE)
          system("sudo killall tor; tor&")
        }
        
        warning(e)
        warning("(retry...)")
        
        res <<- NULL
        invisible(e)
      })
    }
    
    res
  }
  
  res <- get_(addr, use_tor)
  if (http_error(res)) {
    return(
      data.table(
        addr_norm = NA,
        lat = NA,
        lng = NA,
        # no = NA_character_,
        # bheigh2 = NA_character_,
        # bcount2 = NA_character_,
        # baddr2 = NA_character_,
        # bname2 = NA_character_,
        # village = NA_character_,
        # road = NA_character_,
        msg = NA_character_#,
        # precision = NA
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
  out[, c("lat", "lng") := lapply(.SD, as.numeric), .SDcols = c("lat", "lng")]
  
  to_clean <- setdiff(names(out),  c("msg"))
  out[msg == "Over limit", (to_clean) := NA]
  
  if (precise) {
    out[village == "", (to_clean) := NA]
    out[, msg := "Not precise"]
  }
  
  out[, .(addr_norm, lat, lng, msg)]
}
