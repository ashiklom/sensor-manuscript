library(data.table)
flist <- list.files("run-scripts/results")
id.list <- gsub("(.*)[.]csv", "\\1", flist)
sensor.list <- gsub("([[:alnum:]]+(\\.[[:alpha:]]+)?)\\.[[:digit:]]+$", "\\1", id.list)
names(id.list) <- flist
names(sensor.list) <- flist
results.list <- list()
pb <- txtProgressBar(min=1,max=length(flist), style=3)
i <- 0
for(f in flist) {
    i <- i + 1
    setTxtProgressBar(pb, i)
    ff <-file.path("run-scripts/results", f)
    dat <- fread(ff, header=TRUE)
    dat[, sample_id := id.list[f]]
    dat[, sensor := sensor.list[f]]
    if(!("N.mu" %in% names(dat))) {
        cat("\n")
        print(f)
        print("Did not converge")
        next
    }
    results.list[[f]] <- dat
}
close(pb)

simulation.dat <- rbindlist(results.list)
simulation.dat[, V1 := NULL]

# Convert units of Cw and Cm from g cm-2 to g m-2
convert <- names(simulation.dat)[grepl("C[wm]", names(simulation.dat))]
for(j in convert){
    set(simulation.dat, j=j, value=simulation.dat[[j]]*10000)
}

save(results, file="data/simulation.results.RData")

