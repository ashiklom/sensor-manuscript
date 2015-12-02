library(data.table)
flist <- list.files("results")
id.list <- gsub("(.*)[.]csv", "\\1", flist)
sensor.list <- gsub("([[:alnum:]]+(\\.[[:alpha:]]+)?)\\.[[:digit:]]", "\\1", id.list)
names(id.list) <- flist
names(sensor.list) <- flist
results.list <- list()
for(f in flist) {
    print(f)
    ff <-file.path("results", f)
    dat <- fread(ff, header=TRUE)
    dat[, sample_id := id.list[f]]
    dat[, sensor := sensor.list[f]]
    if(!("N.mu" %in% names(dat))) {
        print("Did not converge")
        next
    }
    results.list[[f]] <- dat
}

results <- rbindlist(results.list)
results[, V1 := NULL]
save(results, file="all.results.RData")

