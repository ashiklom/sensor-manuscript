#' ---
#' title: Validation based on spectral comparison
#' author: Alexey Shiklomanov
#' output_format: html_document
#' ---

#' # Introduction
#' One way to validate a model inversion is to compare the model output in 
#' forward mode to the original measurements used to perform the inversion.  
#' This script does just that by running PROSPECT 5 with parameters from the 
#' FFT database inversion and comparing the resultign simulated reflectance and 
#' transmittance with the original measurements. The results of this validation 
#' are presented as a figure displaying the 90th and 95th percentiles of the 
#' error (bias) of the simulated spectra as well as a table displaying relevant 
#' summary statistics (see manuscript text for more details.)

#' # Setup
#' First, we load required packages. `PEcAnRTM` is used to perform simulations 
#' and contains useful information vectors. `ggplot2`, `gridExtra`, and `grid` 
#' are used to draw and arrange the figure. 
library(PEcAnRTM)
library(ggplot2)
library(gridExtra)
library(grid)

#' The data for generating simulated spectra come from the results of the FFT 
#' inversion, which is located in the `all.results.RData`file. The accompanying 
#' FFT data, including all reflectance and transmittance spectra, are in the 
#' `fft.RData` file. Here we load all the data and then merge the FFT results 
#' and metadata, removing non-tree PFTs (shrubs and grasses).

load("curated-leafspec/processed-spec-data/all.results.RData")
load("curated-leafspec/processed-spec-data/fft.RData")
setkey(fft.dat, sample_id)
setkey(results, sample_id)
fft.f <- results[fft.dat][plant_type %in% c("broadleaf", "conifer")]

#' Because they are a physical impossibility and are indicative of measurement 
#' error, we convert all negative transmittance values to `NA`.

fft.transspec[fft.transspec < 0] <- NA

#' # Calculating the error
#' We define a function that takes data tables of parameter inversion estimates 
#' (`dat.mod`) and measured spectra (`dat.obs`) as input and returns a matrix 
#' of error values by wavelength. The argument `refltrans` dictates whether 
#' this is comparing reflectance (1) or transmittance (2) by selecting the 
#' appropriate column from the PROSPECT model output.  First, this function 
#' merges the two input data tables into a single large table (`rt.big`) with 
#' correctly aligned rows based on the sample name and year. We then subset 
#' `rt.big` to only the mean parameter estimates and the measured spectra and 
#' convert the resulting object to a matrix (`rt.mat`). We define a `tdiff` 
#' function that operates on a single row of this matrix, simulating the 
#' spectra based on the PROSPECT parameters (first 5 columns) and subtracting 
#' this from the spectra (remaining columns).  Finally, we apply this function 
#' to the matrix row-by-row, creating a matrix of error values.

parnames <- sprintf("%s.mu", params.prospect5)
params.dat <- fft.f[, parnames, with=F]
sim.dat <- t(apply(params.dat, 1, prospect, version=5))
rownames(sim.dat) <- fft.f[,sample_id]
sim.refl <- sim.dat[id.refl,1:2101]
sim.trans <- sim.dat[id.trans,-2101:0]

id.refl <- intersect(rownames(fft.reflspec), rownames(sim.refl))
id.trans <- intersect(rownames(fft.transspec), rownames(sim.trans))

error.refl <- sim.refl - fft.reflspec[id.refl,-50:0]
error.trans <- sim.trans - fft.transspec[id.trans,-50:0]

id.refl.bl <- intersect(fft.f[plant_type == "broadleaf", sample_id], rownames(error.refl))
id.trans.bl <- intersect(fft.f[plant_type == "broadleaf", sample_id], rownames(error.trans))
id.refl.con <- intersect(fft.f[plant_type == "conifer", sample_id], rownames(error.refl))
id.trans.con <- intersect(fft.f[plant_type == "conifer", sample_id], rownames(error.trans))

error.refl.bl <- error.refl[id.refl.bl,]
error.trans.bl <- error.trans[id.trans.bl,]
error.refl.con <- error.refl[id.refl.con,]
error.trans.con <- error.trans[id.trans.con,]

#' When validating the inversion results of the FFT database, it is important 
#' to distinguish between errors inherent to PROSPECT (which this paper does 
#' not address) from errors arising from PROSPECT inversion (which are more 
#' relevant to this paper). Whereas the above code block prepares data for a 
#' validation against measured spectra, the following code block loads 
#' simulated data used to test the extent to which spectral inversion errors 
#' come from our inversion algorithm rather than inherent issues with PROSPECT.

#' The data for this block comes from the sensor simulation experiment, which 
#' is based on inversion estimates from the FFT database (to preserve inherent 
#' parameter covariances). Each of these is stored in a separate RData file, so 
#' we have to load them separately. To generate the "observed" spectra for 
#' each, we run the PROSPECT model using the true parameter values and then add 
#' the stored noise spectrum. For transmittance, we use the `PEcAnRTM` 
#' `generate.noise` function with the same parameters as reflectance to 
#' generate random noise (for repeatability, we manually seed the random number 
#' generator).  To generate the modeled spectra, we run PROSPECT using the mean 
#' values of the inversion estimates.

#set.seed(777)
#fpath <- "identity-results"
#flist <- list.files(fpath)
#nfiles <- length(flist)
#rem <- matrix(NA, 2101, nfiles)
#tem <- matrix(NA, 2101, nfiles)
#for(f in 1:nfiles){
    #print(sprintf("%d of %d", f, nfiles))
    #fname <- flist[f]
    #f.list <- load.from.name(fname, fpath)
    #obs <- with(f.list, prospect(c(N, Cab, Car, Cw, Cm), 5))
    #obs[,1] <- obs[,1] + f.list$noise
    #obs[,2] <- obs[,2] + generate.noise(fw=11, sigma=0.00025, fsd=2)
    #mod <- with(f.list, prospect(c(N.mu, Cab.mu, Car.mu, Cw.mu, Cm.mu), 5))
    #rem[,f] <- mod[,1] - obs[,1]
    #tem[,f] <- mod[,2] - obs[,2]
#}

#' # Summarizing the error
#' Here, we define a function that takes an error matrix as input and 
#' calculates the mean and 80th and 95th percentiles of the error by 
#' wavelength. It also calculates regions where the 95% confidence interval 
#' does not overlap zero (`shade.max` and `shade.min`, so called because these 
#' regions will be shaded in the plot). The `pft` and `measure` inputs are used 
#' to specify the plant functional type (All, Broadleaf, or Conifer) and the 
#' measurement type (Reflectance or Transmittance), respectively. 

error.data <- function(err.mat, pft, measure){
    err.means <- colMeans(err.mat, na.rm=TRUE)
    err.q <- apply(err.mat, 2, quantile, c(0.025,0.10,0.90,0.975), na.rm=TRUE)
    err.dat <- data.table(wavelength = 400:2500, means=err.means,
                          pft = pft, measure=measure)
    err.dat <- cbind(err.dat, t(err.q))
    setnames(err.dat, rownames(err.q), c("low2", "low1", "high1", "high2"))
    err.dat[, shade.min := 0][, shade.max := 0]
    err.dat[low2 > 0, shade.max := low2]
    err.dat[high2 < 0, shade.min := high2]
    return(err.dat)
}

#' We then generate a list of all the error matrices, along with their 
#' corresponding plant and measurement types, and `mapply` the `error.data` 
#' function to them to generate a list of `data.table`s. We then use the fast 
#' `rbindlist` function to combine the list into a single `data.table`, which 
#' will be used to generate the plot.

em.list <- list(error.refl, error.refl.bl, error.refl.con,
                error.trans, error.trans.bl, error.trans.bl)
pft.names <- c("All", "Broadleaf", "Conifer")
pft.list <- rep(pft.names, 2)
measure.list <- rep(c("Reflectance", "Transmittance"), each=3)
dat.list <- mapply(error.data, em.list, pft.list, measure.list, SIMPLIFY=FALSE)
dat.all <- rbindlist(dat.list)
dat.all[, pft := factor(pft, levels = pft.names)]

#' # Plotting the error
#' Below, we create a function that takes the error data table and a string 
#' describing the facets as input and produces a faceted `ggplot` of the error 
#' as output. Specifically, the plot has a solid black line showing the mean 
#' error, two shaded grey regions for the 90% and 95% confidence intervals, a 
#' red line highlighting zero, and shaded red regions highlighting regions 
#' significantly different from zero.

error.plot <- function(err.dat, facet.string){
    err.spec <- ggplot(err.dat) +
        aes(x=wavelength) +
        geom_ribbon(aes(ymin=low2,ymax=high2), alpha=0.5, 
                    fill="grey", color="black", linetype="dashed", size=0.4) +
        geom_ribbon(aes(ymin=low1,ymax=high1), alpha=0.5, 
                    fill="grey", color="black", linetype="dotted", size=0.4) +
        geom_line(aes(y=means), size=0.6) +
        geom_hline(y=0, color="red", size=0.8) +
        geom_ribbon(aes(ymin=shade.min, ymax=shade.max), fill="red") +
        xlab("Wavelength (nm)") +
        ylab("Error (model - observed)") +
        facet_grid(facet.string, scales="free_y") +
        theme_bw() +
        theme(text = element_text(size=12),
              axis.title = element_text(size=rel(1)),
            axis.text = element_text(size=rel(0.7)))
    return(err.spec)
}
#' Below, we apply our plot function to the data table we generated previously 
#' and save the resulting plot to a PDF.
dat.plot <- error.plot(dat.all, "measure~pft")
png("figures/refltrans-validation.png", width=14, height=14, units="in", res=300, pointsize=25)
plot(dat.plot)
dev.off()

#' Below, we repeat the above analyses, but on the re-simulated spectra.

#simdat.list <- mapply(error.data, list(rem, tem), "All", 
                      #c("Reflectance", "Transmittance"),
                      #SIMPLIFY=FALSE)
#simdat <- rbindlist(simdat.list)
#sim.plot <- error.plot(simdat, "measure~.")
#pdf("manuscript/figures/sim-refltrans-validation.pdf", width=3, height=4)
#plot(sim.plot)
#dev.off()


#' # Error statistics table
#' To facilitate comparison with other studies and due to their inherent 
#' differences, we compute statistics separately for the visible (VIS) and near 
#' infrared (NIR) regions of the spectrum, defined here.

#vis.wl <- 400:800
#nir.wl <- 801:2500

#' We define a function that takes a matrix as input and returns a vector 
#' containing the values of error statistics as output. For each statistic, we 
#' first compute the statistic for each wavelength and then calculate a mean 
#' according the spectral region of interest.

#rmse.wl <- function(mat){
    #mat <- t(mat)
    #i.vis <- vis.wl - 399
    #i.nir <- nir.wl - 399
    #rmse <- apply(mat, 2, function(x) sqrt(mean(x^2, na.rm=TRUE)))
    #rmse.vis <- mean(rmse[i.vis], na.rm=TRUE)
    #rmse.nir <- mean(rmse[i.nir], na.rm=TRUE)
    #bias <- apply(mat, 2, mean, na.rm=TRUE)
    #bias.vis <- mean(bias[i.vis], na.rm=TRUE)
    #bias.nir <- mean(bias[i.nir], na.rm=TRUE)
    #err.c <- t(apply(mat, 1, "+", -bias))
    #sepc <- apply(err.c, 2, function(x) sqrt(mean(x^2, na.rm=TRUE)))
    #sepc.vis <- mean(sepc[i.vis], na.rm=TRUE)
    #sepc.nir <- mean(sepc[i.nir], na.rm=TRUE)
    #out <- c("rmse.vis"=rmse.vis,
             #"bias.vis"=bias.vis,
             #"sepc.vis"=sepc.vis,
             #"rmse.nir"=rmse.nir,
             #"bias.nir"=bias.nir,
             #"sepc.nir"=sepc.nir)
    #return(out)
#}

##' We then apply the above function to each error matrix, using `lapply` and 
##' `cbind` similarly to what we did above.

#cnames <- sprintf("%s-%s", measure.list, pft.list)
#names(em.list) <- cnames
#rmse.full<- lapply(em.list, rmse.wl)
#sumtab <- do.call(cbind, rmse.full)
#rownames(sumtab) <- c("VIS-RMSE", "VIS-BIAS", "VIS-SEPC",
                      #"IR-RMSE", "IR-BIAS", "IR-SEPC")
#tsumtab <- t(sumtab)
#print("Reflectance and transmittance validation: Table of summary statistics")
#print(tsumtab, digits=2)
