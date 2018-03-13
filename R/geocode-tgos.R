#' @import magrittr httr rvest stringr data.table
geocode_tgos_ <- function(addr, keystr, 
                          rate = 200, use_tor = TRUE, max_try = 3,
                          ...) {
  # addr = "臺北市大安區光復南路302"
  
  ## to be nice :)
  Sys.sleep(rexp(1, rate)) # sleep expo dist at rate per sec
  
  ua <- user_agent("Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36")
  url <- "https://gis.tgos.tw/TGLocator/TGLocator.ashx"
  
  if (use_tor) {
    old <- set_config(use_proxy("socks5://localhost:9050"))
    on.exit({
      set_config(old, override = TRUE)
    })
    # set_config(verbose())
  }
  
  i <- 1
  while (i <= max_try) {
    tryCatch({
      res <- GET(url,
                 ua,
                 add_headers(
                   Referer = "https://map.tgos.tw/TGOSimpleViewer/Web/Map/TGOSimpleViewer_Map.aspx"
                 ),
                 query = 
                   list(
                     format = "json",
                     input = addr,
                     srs = I("EPSG:4326"),
                     keystr = keystr
                   )
      )
      out <- res %>% content(as = "parsed", type = "application/json")
      
      if (! is.null(out$error_message)) {
        message("(Invalid key. Renewing the key...)")
        keystr <- get_keystr()
        next
      }
      
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
  
  data.table(
    lng = out$results[[1]]$geometry$x,
    lat = out$results[[1]]$geometry$y,
    addr_norm = out$results[[1]]$FULL_ADDR,
    msg = out$status
  )
}


#' @import magrittr httr rvest stringr data.table
get_keystr <- function() {
  
  ua <- user_agent("Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36")
  
  session_id <- GET("https://map.tgos.tw/TGOSimpleViewer/Web/Map/TGOSimpleViewer_Map.aspx") %>% 
    cookies %>% 
    .[["value"]]
  
  res_getJSAPI <- GET(
    "https://map.tgos.tw/TGOSimpleViewer/Web/getJSAPI.aspx?key=tgos2d",
    ua,
    set_cookies(
      ASP.NET_SessionId = session_id
    ),
    add_headers(
      Referer = "https://map.tgos.tw/TGOSimpleViewer/Web/Map/TGOSimpleViewer_Map.aspx"
    )
  )
  keystr <- res_getJSAPI %>% content("text") %>% str_match('TGOS\\.tgHash="([^"]+)"') %>% .[,2]
  keystr
}
