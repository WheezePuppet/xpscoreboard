
library(dplyr)
library(xtable)

source("db.R")

source("mysql_config.R")

xp <- function(partial.realname, pts=1, tag="misc") {
    require(RMySQL)
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
        charname <- 
            chars[grep(partial.realname,chars$realname),"charname"][[1]][1]
        old.num.rows <- nrow(filter(xp,username==charname,xps==pts,tag==tag))
        conn <- get.connection(TRUE)
        dbGetQuery(conn,
            paste0("insert into xp values (",
            "'",charname,"',",pts,",'",tag,"',now())")
        )
        dbDisconnect(conn)
        xp <- collect(tbl(db.src, "xp"))
        new.num.rows <- nrow(filter(xp,username==charname,xps==pts,tag==tag))
        if (new.num.rows == old.num.rows + 1) {
            return("Success.")
        } else {
            return("Nothing written!")
        }
    }, error=function(e) e)
}
