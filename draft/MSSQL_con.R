library(RSQLServer)
library(DBI)
library(dplyr)
library(dtplyr)
library(data.table)
library(stringr)
library(addr2gps)

# DB con ------------------------------------------------------------------

con <- dbConnect(RSQLServer::SQLServer(), # jTDS driver 
                 "TSDB_MSSQL-connection", 
                 database = 'TSDB_MSSQL')

## List tables
dbListTables(con)

# ## List table fields
# dbListFields(con, 'mtcars')

# ## Fetch all results
# res <- dbSendQuery(con, "SELECT * FROM information_schema.tables")
# dbFetch(res)
# dbClearResult(res)
# 
# ## Send and Fetch query result
# dbGetQuery(con, "SELECT * FROM information_schema.tables")


# get data ---------------------------------------------------------

## dplyr connection
db <- RSQLServer::src_sqlserver("TSDB_MSSQL-connection", database = "TSDB_MSSQL")
company <- tbl(db, sql("SELECT * FROM COMPANY"))

## 只選台中
taizhong <- company %>% 
  select(addr) %>% 
  filter(str_addr %>% str_detect("^台中")) %>% 

# Go ----------------------------------------------------------------------

bank <- bank %>% collect() %>% tbl_dt()
bank %>% setnames("地址", "addr")

system.time({
  res <- bank[, addr] %>% unique %>% get_gps()
})

#  user  system elapsed 
# 0.364   0.164 181.497 

# Merge -------------------------------------------------------------------

bank_gps <- merge(bank, res, by = "addr", all.x = TRUE)

# Write DB ----------------------------------------------------------------

system.time({
  dbWriteTable(con, name = "BANK_GPS", bank_gps)
})

#   user  system elapsed 
# 13.548   0.328 281.525


