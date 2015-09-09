# Run all analyses in order

# Methods
#source("methods.prospect-sensitivity.R")

# Results
results.files <- list.files(".", "results\\..*\\.R")
for(f in results.files){
    print(f)
    source(f)
}

## Validation
#source("results.validation-refltrans-error.R")
#source("results.water-lma-figure-table.R")

## Sensor analysis
#source("results.sensor-errorstat-table.R")
#source("results.sensor-error-plot.R")
#source("results.sensor-covariance.R")

# Compile tables and move to Google Drive
system('cd manuscript/tables; pdflatex tables.tex; latexmk -c;\
       mv tables.pdf ../drive-folder')
