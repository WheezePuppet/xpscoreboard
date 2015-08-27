
require(RMySQL)

get.connection <- function(force.new=FALSE) {
    if (!exists("mysql.db.name")) {
        source("mysql_config.R")
    }
    if (force.new || !exists("conn") || is.null(conn)) {
        conn <<- dbConnect(MySQL(max.con=100),mysql.db.name,
            username=mysql.user,password=mysql.password)
    }
    conn
}
