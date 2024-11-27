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

## Opret tabel med de tre forhandlere
forhandler <- data.frame(matrix(nrow = 4, ncol = 3))
colnames(forhandler) <- c("forhandlernavn", "adresse", "cvr")

forhandler$forhandlernavn <- c("Kraft Biler A/S", "Brdr´s Auto Taastrup ApS", "Bilhuset Jylland ApS", "Test")
forhandler$adresse <- c("Navervej 2-4, 7000 Fredericia", "Rugvænget 1, 2630 Taastrup","Soldalen 2, 7100 Vejle","Test")
forhandler$cvr <- c(28859309, 34897085, 44934442, 0)

## Importer forhandlerdata
dbWriteTable(con, "forhandler", forhandler, overwrite = FALSE, append = TRUE)


#### Tabel med stamdata ####
tesla_stam <- tesla_clean[,c(2,3,5,6, 8:11)]

#### Tabel med variabel data ####
tesla_var <- tesla_clean[,c(6,1,7)]
tesla_var$sold <- F


## Importer tesla_stam
dbWriteTable(con, "tesla", tesla_stam, overwrite = FALSE, append = TRUE)

## Importer tesla_var
dbWriteTable(con, "tesla_var", tesla_var, overwrite = FALSE, append = TRUE)
