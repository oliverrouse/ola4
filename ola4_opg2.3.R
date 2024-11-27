## Pakker
library(DBI)
library(RMariaDB)
library(keyring)

## Connection
key_set("my_password", "MySQL")
password <- key_get("my_password", "MySQL")

con <- dbConnect(MariaDB(),
                 host="localhost",
                 db="biler",
                 user="root",
                 password = password)


#### Tesla sim tabeller ####
tesla_sim_stam <- tesla_sim[,c(2,3,5,6, 8:11)]
tesla_sim_var <- tesla_sim[,c(6,1,7)]


## Importer tesla_sim_stam
dbWriteTable(con, "tesla_sim_stam", tesla_sim_stam, overwrite = FALSE, append = TRUE)

## Importer tesla_var
dbWriteTable(con, "tesla_sim_var", tesla_sim_var, overwrite = FALSE, append = TRUE)

