#' @param data data.frame contains address variable.
#' @param addr_var Character. Name of address variable.
#'
#' @describeIn geocode Add GIS columns to data.frame.
#' 
#' @examples 
#' library(data.table)
#' library(magrittr)
#' 
#' dt <- data.table(addrs = c("台北市中正區羅斯福路一段２號",
#'                            "台北市中正區貴陽街一段１２０號"),
#'                  X = c(3, 2))
#' out <- dt %>% add_geocode(addr_var = "addrs", use_tor = FALSE)
#' out
#' 
#' @export
add_geocode <- function(data, addr_var, 
                        precise = FALSE, source = c("map.com", "tgos", "mix"), 
                        parallel = TRUE, n_cpu = -1L, 
                        rate = 200, use_tor = TRUE) {
  
  source <- match.arg(source)                        
  
  setDT(data)
  
  matched_addrs <- data[[addr_var]] %>%
    geocode(source = source, 
            precise = precise, 
            n_cpu = n_cpu, 
            rate = rate, 
            use_tor = use_tor) %>% 
    .[, .(addr, addr_norm, lat, lng)]
  
  out <- matched_addrs[data, on = c(addr = addr_var)]
  setnames(out, old = "addr", new = addr_var)
  
  out[]
}
