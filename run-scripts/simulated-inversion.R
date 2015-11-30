# Arguments : Index, sensor, run.name
library(PEcAnRTM)
data(sensor.rsr)
arg <- commandArgs(trailingOnly=TRUE)
if(length(arg) == 0) arg <- c("1", "chris.proba", "testrun")

# Load inputs and arguments
load("simulation.inputs.RData")
parnames <- params.prospect5
index <- as.numeric(arg[1])
true.params <- par.mat[index,]
names(true.params) <- parnames
obs.raw <- obs.mat[,index]
noise <- noise.mat[,index]
sensor <- arg[2]
run.name <- arg[3]

# Inversion parameters
observed <- spectral.response(obs.raw, sensor)
ngibbs <- 100000
burnin <- 80000
nchains <- 5
n.tries <- 5
return.samples <- TRUE
version <- 5
target <- 0.234
target.adj <- 0.8
do.lsq.first <- FALSE
do.lsq.after <- 3
save.samples <- NULL
quiet <- TRUE

# Get column names from summary.simple function (a bit of a hack)
samps <- matrix(0, nrow=1, ncol=6)
colnames(samps) <- c(params.prospect5, "residual")
samps.summary <- summary.simple(samps)
cnames <- names(samps.summary)

# Set up custom PROSPECT inversion parameters
model <- function(param) spectral.response(prospect(param, version)[,1], sensor)
prior.params <- prior.defaultvals.prospect(sd.inflate = 3)
prior.function <- with(prior.params, priorfunc.prospect(mu, sigma))
pm <- c(1, 0, 0, 0, 0)
inits.function <- function(){
    inits <- with(prior.params, rlnorm(5, mu, sigma))
    inits[1] <- inits[1] + 1
    names(inits) <- params.prospect5
    return(inits)
}

out <- invert.auto(observed = observed,
                   model = model,
                   ngibbs = ngibbs,
                   nchains = nchains,
                   prior.function = prior.function,
                   inits.function = inits.function,
                   param.mins = pm,
                   burnin = burnin,
                   n.tries = n.tries,
                   return.samples = return.samples,
                   target.adj = target.adj,
                   do.lsq.first = do.lsq.first,
                   do.lsq.after = do.lsq.after,
                   save.samples = save.samples)

results <- c(true.params, out$results)
samples <- out$samples
write.csv(results, file=sprintf("results/%s.csv", run.name))
save(samples, file=sprintf("samples/%s.RData", run.name))
