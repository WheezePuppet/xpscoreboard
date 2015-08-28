
library(dplyr)
library(xtable)

source("db.R")

xp <- function(charname, pts=1, tag="misc") {
    tryCatch({
        dbGetQuery(get.connection(),
            paste0("insert into xp values (",
            "'",charname,"',",pts,",'",tag,"',now())")
        )
    }, error=function(e) e)
}
