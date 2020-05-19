library(RPostgreSQL)

start <- function(){
    db_name <- "metrics"
    host <- "localhost"
    db_port <- 5432
    db_user <- "testuser"
    db_pass <- "pass"
    conn <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
    return(conn)
}

stop <- function(db){
    dbDisconnect(db)
    return(0)
}

query <- function(db, q){
    start_time <- Sys.time()
    res <- dbGetQuery(db, q)
    end_time <- Sys.time()
    print(q)
    print(end_time - start_time)
    return(res)
}
