loadUnitNames <- function(db, unames) {
    dbBeginTransaction(db)
    ## To use an auto-incrementing field via RSQLite, you need
    ## to be careful to pass integer NA's
    df <- data.frame(id=rep(as.integer(NA), length(unames)), name=unames)
    values <- "(:id, :name)"
    sql <- "insert into featureSet (fsetid, man_fsetid) values"
    dbGetQuery(db, paste(sql, values), bind.data=df)
    dbCommit(db)
}

loadUnits <- function(db, batch) {
    batchMat <- do.call(rbind, lapply(batch, pmmmBlockToMat))

    loadUnitNames(db, names(batch))
    
    ## Find internal featureSet IDs for these features
    batchLens <- sapply(batch, function(x)
                        sum(sapply(x$groups, function(y)
                                   length(y$indices))))
    sql <- paste("select fsetid from featureSet where man_fsetid in (",
                 paste('"', names(batch), '"', sep="", collapse=","),
                 ")")
    batchIds <- dbGetQuery(db, sql)[[1]]
    ## XXX: We divide batchLens by 2 because of the stacked pm/mm
    ##      format of the data.  Within a unit (element of batch above)
    ##      everything has the same featureSet name and half are pm,
    ##      half mm.  The data gets "stacked" and for efficiency
    ##      reasons, we don't unstack it here.  So we div by 2.
    batchIds <- rep(batchIds, batchLens/2)
    batchMat <- cbind(batchMat, fsetid=batchIds)

    ## Insert pm and mm into respective tables
    pmCols <- c(seq.int(1, 12, by=2), 13:13)
    mmCols <- c(seq.int(2, 12, by=2), 13:13)
    values <- "(:indices, :strand, :allele, :fsetid, :expos, :x, :y)"
    sql <- paste("insert into pmfeature values", values)
    dbBeginTransaction(db)
    rset <- dbSendPreparedQuery(db, sql, as.data.frame(batchMat[, pmCols]))
    dbClearResult(rset)

    sql <- paste("insert into mmfeature values", values)
    rset <- dbSendPreparedQuery(db, sql, as.data.frame(batchMat[, mmCols]))
    dbClearResult(rset)
    dbCommit(db)
}


loadUnitsByBatch <- function(db, cdfFile, batch_size=10000,
                             max_units=NULL, verbose=FALSE) {
    unames <- readCdfUnitNames(cdfFile)
    offset <- 1
    extra <- length(unames) %% batch_size
    num_batches <- length(unames) %/% batch_size
    if (extra != 0)
      num_batches <- num_batches + 1
    done <- FALSE
    if (is.null(max_units))
      max_units <- length(unames)
    while (!done) {
        end <- min(offset + batch_size, max_units)
        if (end == max_units)
          done <- TRUE
        wanted <- seq.int(offset, end)
        offset <- offset + batch_size + 1
        vvunits <- readCdfUnits(cdfFile, units=wanted, stratifyBy="pmmm",
                                readIndices=TRUE)
        loadUnits(db, vvunits)
    }
}


loadAffyCsv <- function(db, csvFile, batch_size=5000) {
    csvFile <- "AffyMapping500k-data/Mapping250K_Nsp_annot.csv"
    con <- file(csvFile, open="r")
    on.exit(close(con))

    wantedCols <- c(1,2,3,4,7,8,12,13)
    colClasses <- rep("character", 26)
    colClasses[c(2, 7)] <- "integer"
    df <- read.table(con, sep=",", stringsAsFactors=FALSE, nrows=10,
                     na.strings="---", colClasses=colClasses,
                     header=TRUE)[, wantedCols]
    header <- gsub(".", "_", names(df), fixed=TRUE)
    
    db_cols <- c("affy_snp_ip", "dbsnp_rs_id", "chrom",
                 "phsyical_pos", "strand", "allele_a", "allele_b")
    
    val_holders <- c(":AFFY_SNP_ID", ":dbSNP_RS_ID", ":Chromosome",
                     ":Physical_position", ":Strand", ":Allele_a",
                     ":Allele_b")

    exprs <- paste(db_cols, " = '", val_holders, "'", sep="", collapse=", ")
    sql <- paste("update featureSet set ", exprs,
                 "where man_fsetid = ':Probe_Set_ID'")

    dbBeginTransaction(db)
    dbGetQuery(db, sql, bind.data=df)
    dbCommit(db)

    ## Now do the rest in batches
    done <- FALSE
    while (!done) {
        df <- read.table(con, sep=",", stringsAsFactors=FALSE,
                         nrows=batch_size, na.strings="---",
                         colClasses=colClasses, header=FALSE)[, wantedCols]
        if (nrow(df) < batch_size) {
            done <- TRUE
            if (nrow(df) == 0)
              break
        }
        names(df) <- header
        dbBeginTransaction(db)
        dbGetQuery(db, sql, bind.data=df)
        dbCommit(db)
    }
}
