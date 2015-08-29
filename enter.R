
library(dplyr)
library(xtable)

source("db.R")

source("mysql_config.R")

xp <- function(partial.realname, pts=1, tag="misc") {
    tryCatch({
        db.src <- src_mysql(mysql.db.name,user=mysql.user,
            password=mysql.password)
        chars <- collect(tbl(db.src, "chars"))
        xp <- collect(tbl(db.src, "xp"))
        if (length(grep(partial.realname,chars$realname)) > 1) {
            stop(paste0("More than one realname match for ",
                partial.realname, "."))
        }
        if (length(grep(partial.realname,chars$realname)) < 1) {
            stop(paste0("No realname match for ",
                partial.realname, "."))
        }
        charname <- chars[grep(partial.realname,chars$realname),"charname"]
        dbGetQuery(get.connection(TRUE),
            paste0("insert into xp values (",
            "'",charname,"',",pts,",'",tag,"',now())")
        )
    }, error=function(e) e)
    "Success."
}
