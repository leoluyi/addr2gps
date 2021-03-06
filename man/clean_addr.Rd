% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_addr.R
\name{clean_addr}
\alias{clean_addr}
\title{Clean address}
\usage{
clean_addr(addrs)
}
\arguments{
\item{addrs}{Address vecter}
}
\value{
Character vector of cleaned addrs
}
\description{
Clean address
}
\details{
\itemize{
  \item 全形及半形空格(如：`台北市　民生東路五段`)
  \item 簡寫(如：`北市中山路`)，城市縮寫處理請看下頁
  \item 行政區路名(如：`美村路`)
  \item 可辨識新舊五都(如：台北縣、新北市…等等)
  \item 可辨識全形、半形、國字數字及阿拉伯數字(如：`一巷`、`1巷`等等)
  \item 連號問題(如：`1-3號`、`1，3號`等等)，辨識至特殊符號前第一個號碼
        (如：`台中市西屯區台中港路三段100號1~5樓，100-1及102號1~3樓`)
  \item 地址字串中需排除非地址使用的特殊符號與空白等
  \item 辨識至最小地址單位(如：`台北市松山區八德路四段138號3樓球體區`，辨識至「號」)
}
}
\examples{
clean_addr("台北市松山區八德路四段138號3樓球體區")

}
\references{
\url{https://github.com/moskytw/zipcodetw}
}
