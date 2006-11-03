initDb <- function(dbname) {
    db <- dbConnect(dbDriver("SQLite"), dbname)
    
    ## Create tables
    rset <- dbSendQuery(db, createFeatureSetSql)
    dbClearResult(rset)
    
    rset <- dbSendQuery(db, sprintf(createFeatureSql, "pmfeature"))
    dbClearResult(rset)
    
    rset <- dbSendQuery(db, sprintf(createFeatureSql, "mmfeature"))
    dbClearResult(rset)

    sql <- 'create index man_fsetid_idx on featureSet ("man_fsetid")'
    dbGetQuery(db, sql)
    db
}

closeDb <- function(db) dbDisconnect(db)
