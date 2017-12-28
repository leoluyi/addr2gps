library(data.table)
library(httr)
library(rvest)
library(stringr)

ua = user_agent("Mozilla/5.0 (Windows NT 6.3; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.84 Safari/537.36")
url = "https://gis.tgos.tw/TGLocator/TGLocator.ashx"


get_keystr <- function() {
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

geocode_.tgos <- function(addr, keystr, use_tor = TRUE) {
  if (use_tor) {
    old <- set_config(use_proxy("socks5://localhost:9050"))
    on.exit({
      set_config(old, override = TRUE)
    })
    # set_config(verbose())
  }
  
  res <- GET(url,
             ua,
             add_headers(
               # Host = "gis.tgos.tw",
               # `Accept-Encoding` = "gzip, deflate, br",
               # `Accept-Language` = "zh-TW,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-CN;q=0.6,ja;q=0.5",
               # Connection = "keep-alive",
               Referer = "https://map.tgos.tw/TGOSimpleViewer/Web/Map/TGOSimpleViewer_Map.aspx"
             ),
             # set_cookies(
             #    `_ga` = "GA1.2.14420987.1514444967",
             #    `_gid`= "GA1.2.14420987.1514444967",
             #    `_gat` = "1"
             # ),
             query = 
               list(
                 format = "json",
                 input = addr,
                 srs = I("EPSG:4326"),
                 keystr = keystr
               )
  )
  res %>% content(as = "parsed", type = "application/json")
}

testit <- function(ITER = 200000) {
  tic <- Sys.time()
  keystr <- get_keystr()
  
  # ITER = 200000
  pb <- txtProgressBar(min = 0, max = ITER, initial = 0, style = 3) 
  i = 1
  while (i <= ITER) {
    setTxtProgressBar(pb, i)
    out_ls <- geocode_.tgos("臺北市大安區光復南路302", keystr)
    if (! is.null(out_ls$error_message)) {
      message("Invalid key at iter: ", i)
      keystr <- get_keystr()
      next
      # break
    }
    
    time_diff <- as.numeric(difftime(Sys.time(), tic, units = "sec"))
    
    cat(paste0("\r(iter: ", i, ") speed: ", i / time_diff, " iter/sec"))
    i = i+1
  }
  close(pb)
}
testit()
# speed: 1.64135742417298 iter/sec
#! sudo killall tor; tor&
