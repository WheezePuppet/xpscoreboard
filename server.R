
library(shiny)
library(dplyr)
library(xtable)

source("mysql_config.R")

shinyServer(function(input,output,session) {

    INFLATION.ADJUSTMENT <- 0
    MAX.OUT.PTS <- 1100

    compute.level <- function(xp) {
        levels <- c(
                    "Alan Turing"=500,
                    "Ada Lovelace"=480,
                    "Allen, Fran"=440,
                    "Boole, George"=400,
                    "Babbage, Charles"=370,
                    "Bertrand Russell"=340,
                    "Church, Alonzo"=320,
                    "Cantor, Georg"=300,
                    "Chomsky, Noam"=280,
                    "Donald Knuth"=260,
                    "David Hilbert"=240,
                    "Davis, Martin"=220,
                    "Kurt G&ouml;del"=200,
                    "Johnny von Neumann"=180,
                    "Srinivasa Ramanujan"=160,
                    "R&oacute;zsa P&eacute;ter"=140,
                    "Muhammad Musa Al-Khwarizimi"=120,
                    "Barbara Liskov"=100,
                    "Stephen Kleene"= 80,
                    "Julia Robinson"= 65,
                    "Sally Floyd"= 50,
                    "Emil Post"= 35,
                    "Jake Feinler"= 20,
                    "Gottlob Frege"= 10,
                    "Dead battery"=0)

        names(levels[xp >= levels][1])
    }

    compute.image <- function(xp) {
        levels <- c(
                    "turing.jpg"=500,
                    "ada.jpg"=480,
                    "allen.jpg"=440,
                    "boole.jpg"=400,
                    "babbage.jpg"=370,
                    "russell.jpg"=340,
                    "church.jpg"=320,
                    "cantor.jpg"=300,
                    "chomsky.jpg"=280,
                    "knuth.jpg"=260,
                    "hilbert.jpg"=240,
                    "davis.jpg"=220,
                    "godel.jpg"=200,
                    "vonneumann.jpg"=180,
                    "ramanujan.jpg"=160,
                    "peter.jpg"=140,
                    "alkhwarizimi.jpg"=120,
                    "liskov.jpg"=100,
                    "kleene.jpg"=80,
                    "robinson.jpg"=65,
                    "floyd.jpg"=50,
                    "post.jpg"=35,
                    "feinler.jpg"=20,
                    "frege.jpg"=10,
                    "deadBattery.png"=0)

        names(levels[xp >= levels][1])
    }

    compute.url <- function(xp) {
        levels <- c(
                    "Alan_Turing"=500,
                    "Ada_Lovelace"=480,
                    "Frances_E._Allen"=440,
                    "George_Boole"=400,
                    "Charles_Babbage"=370,
                    "Bertrand_Russell"=340,
                    "Alonzo_Church"=320,
                    "Georg_Cantor"=300,
                    "Noam_Chomsky"=280,
                    "Donald_Knuth"=260,
                    "David_Hilbert"=240,
                    "Martin_Davis_(mathematician)"=220,
                    "Kurt_G%C3%B6del"=200,
                    "John_von_Neumann"=180,
                    "Srinivasa_Ramanujan"=160,
                    "R%C3%B3zsa_P%C3%A9ter"=140,
                    "Muhammad_ibn_Musa_al-Khwarizmi"=120,
                    "Barbara_Liskov"=100,
                    "Stephen_Kleene"=80,
                    "Julia_Robinson"=65,
                    "Sally_Floyd"=50,
                    "Emil_Post"=35,
                    "Elizabeth_J._Feinler"=20,
                    "Gottlob_Frege"=10,
                    "Electric_battery"=0)

        names(levels[xp >= levels][1])
    }

    compute.grade <- function(xp) {
        levels <- c("A+"=500,
                    "A"=480,
                    "A-"=440,
                    "B+"=400,
                    "B"=370,
                    "B-"=340,
                    "C+"=320,
                    "C"=300,
                    "C-"=280,
                    "D+"=260,
                    "D"=240,
                    "D-"=220,
                    "F"=0)
                    
        names(levels[xp >= levels][1])
    }

    compute.score <- function(pts) {
        if (pts+INFLATION.ADJUSTMENT >= MAX.OUT.PTS) {
            return("enough")
        } else {
            return(as.character(as.integer(pts)+INFLATION.ADJUSTMENT))
        }
    }

    output$xpPlot <- renderTable({

        input$addchar
        db.src <- src_mysql(mysql.db.name,user=mysql.user,
            password=mysql.password)
        chars <- collect(tbl(db.src, paste0("chars_", mysql.db.table.suffix)))
        xp <- collect(tbl(db.src, paste0("xp_", mysql.db.table.suffix)))
        secret <- collect(tbl(db.src, paste0("secret_", mysql.db.table.suffix)))
        display <- inner_join(chars,xp,by=c("charname"="username"))
        if (!is.null(input$app_hash) && input$app_hash == paste0("#",
            secret[1,1])) {
            display <- display %>% group_by(realname,charname)
            display <- display %>%
                summarize(Grade=compute.grade(sum(xps)),
                Level=paste0("<a href=\"https://en.wikipedia.org/wiki/",
                    compute.url(sum(xps)),
                    "\"><img width=44 src=\"http://stephendavies.org/cpsc326/",
                    compute.image(sum(xps)), "\" /></a> &nbsp; ",
                    "<a href=\"https://en.wikipedia.org/wiki/",
                    compute.url(sum(xps)), "\">",
                    compute.level(sum(xps)),
                    "</a>"),
                XP=compute.score(sum(xps)),
                "Most recent experience"=tag[thetime==max(thetime)][1],
                "Entered"=max(thetime)) %>% rename(Name=realname)
        } else {
            display <- display %>% group_by(charname)
            display <- display %>%
                summarize(
                Level=paste0("<a href=\"https://en.wikipedia.org/wiki/",
                    compute.url(sum(xps)),
                    "\"><img width=44 src=\"http://stephendavies.org/cpsc326/",
                    compute.image(sum(xps)), "\" /></a> &nbsp; ",
                    "<a href=\"https://en.wikipedia.org/wiki/",
                    compute.url(sum(xps)), "\">",
                    compute.level(sum(xps)),
                    "</a>"),
                XP=compute.score(sum(xps)),
                "Most recent experience"=tag[thetime==max(thetime)][1],
                "Entered"=max(thetime)) %>% rename(Name=charname)
        }
        display <- rbind(dplyr::filter(display, XP=="enough"),
            dplyr::filter(display, XP!="enough") %>%
            arrange(desc(as.integer(XP))))

        xtable(as.data.frame(display))
    }, sanitize.text.function = function(x) x)

    output$msg <- renderText({
        if (input$addchar == 0) {
            ""
        } else {
            isolate({
                tryCatch({
                    if (length(grep("^([[:alpha:]]|[[:digit:]]| |-)*$",
                        c(input$realname, input$charname))) != 2) {
                        stop("(Only numbers, digits, and hyphens please!)")
                    }
                    conn <- get.connection(TRUE)
                    dbGetQuery(conn,
                        paste0("insert into chars_", mysql.db.table.suffix,
                        " values (",
                        "'",input$realname,"',",
                        "'",input$charname,"')")
                    )
                    dbGetQuery(conn,
                        paste0("insert into xp_", mysql.db.table.suffix,
                        " values (",
                        "'",input$charname,"',1,'sign-up',now())")
                    )
                    dbDisconnect(conn)
                    "Account created!"
                }, error=function(e) paste0("Could not create account! ",
                    conditionMessage(e)))
            })
        }
    })

    output$myxpstuff <- renderUI({
        div(
            textInput("myxprealname",
                label=HTML("Your real life name (<i>e.g.</i>, Alonzo Church)"),
                value=""),
            textInput("myxpcharname",
                label=HTML("Your screen name (<i>e.g.</i>, lambdaCalc4me)"),
                value=""),
            actionButton("viewxp",label="View XP")
        )
    })

    observeEvent(input$viewxp, {
        
        db.src <- src_mysql(mysql.db.name,user=mysql.user,
            password=mysql.password)
        secret <- collect(tbl(db.src, paste0("secret_", 
            mysql.db.table.suffix)))
        chars <- collect(tbl(db.src, paste0("chars_", 
            mysql.db.table.suffix)))
        xp <- collect(tbl(db.src, paste0("xp_", mysql.db.table.suffix)))

        entered.real.name <- input$myxprealname
        entered.char.name <- input$myxpcharname

        display <- inner_join(chars,xp,by=c("charname"="username"))
        if (!is.null(input$app_hash) && input$app_hash == paste0("#",
            secret[1,1])) {

            entered.char.name <- (chars %>%
                dplyr::filter(realname==entered.real.name) %>% 
                select(charname))[[1]]
        }

        if (nchar(entered.real.name) == 0 ||
            nchar(entered.char.name) == 0) {
            output$myxpmsg <- renderText({ 
                "Enter your real name and character name."
            })
            return(NULL)
        }

        display <- inner_join(chars,xp,by=c("charname"="username")) %>%
            dplyr::filter(charname==entered.char.name,
                   realname==entered.real.name) %>%
            arrange(thetime) %>%
            transmute(Experience=tag,XP=xps,Entered=thetime)

        if (nrow(display) == 0) {
            if (nrow(chars %>% dplyr::filter(charname==entered.char.name)) 
                                                                        == 0){
                output$myxpmsg <- renderText({ 
                    paste0("No such character '",entered.char.name,"'.")
                })
            } else if (nrow(chars %>% 
                    dplyr::filter(realname==entered.real.name)) 
                                                                        == 0){
                output$myxpmsg <- renderText({ 
                    paste0("No such student '",entered.real.name,"'.")
                })
            } else {
                output$myxpmsg <- renderText({ 
                    "Wrong character name!"
                })
            }
            return(NULL)
        }

        output$myxpPlot <- renderTable({
            xtable(as.data.frame(display))
        })

        output$myxpstuff <- renderUI({
            div(
                h3(paste("Experience for",input$myxprealname))
            )
        })
    })
})
