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

#' The data for generating simulated spectra come from the `fft.f` object from 
#' the `FFT.processed.RData` file (for details, see `fft.load.R` and 
#' `fft.preprocess.R`). We then subset `fft.f` to only the full spectra, where 
#' inversion parameter estimates are not `NA`, and only for tree species (i.e.  
#' excluding shrubs and grasses).

load("data/FFT.processed.RData")
fft.f <- fft.f[sensor=="identity"][!is.na(N.mu)][plant.type %in% c("hardwood", "conifer")]
rm(fft.h, fft.c)

#' We then load the observed transmittance data. These data are stored in a 
#' private Dropbox and are available on request. To accelerate and facilitate 
#' loading and pre-processing, we only load a subset of the columns, indicated 
#' by `keep.cols`.

keep.wl <- sprintf("Wave_%d", 400:2500)
keep.other <- c("Spectra", "Species", "Sample_Name", "Sample_Year")
keep.cols <- c(keep.wl, keep.other)
dropbox.path <- "~/Dropbox"
trans.path <- file.path(dropbox.path,
                        "NASA_TE_PEcAn-RTM_Project",
                        "Data", "Spectra",
                        "NASA_FFT_Spectra",
                        "NASA_FFT_IS_Tran_Spectra_v4.csv")
trans <- fread(trans.path, header=TRUE, select=keep.cols)

#' Because they are a physical impossibility and are indicative of measurement 
#' error, we convert all negative transmittance values to `NA`.

remove.negatives <- function(x){
    if(is.numeric(x)) x[x < 0] <- NA
    return(x)
}
trans <- trans[, lapply(.SD, remove.negatives)]

#' We then load the reflectance data, stored in the same place.

refl.path <- file.path(dropbox.path,
                        "NASA_TE_PEcAn-RTM_Project",
                        "Data", "Spectra",
                        "NASA_FFT_Spectra",
                        "NASA_FFT_LC_Refl_Spectra_v4.csv")
refl <- fread(refl.path, header=TRUE, select=keep.cols)

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

error.matrix <- function(dat.mod, dat.obs, refltrans=2){
    setkey(dat.mod, Sample_Name, Sample_Year)
    setkey(dat.obs, Sample_Name, Sample_Year)
    rt.big <- dat.mod[dat.obs][!is.na(N.mu)]
    parnames <- sprintf("%s.mu", params.prospect5)
    rt.mat <- as.matrix(rt.big[,c(parnames, keep.wl), with=FALSE])
    tdiff <- function(mrow){
        pars <- mrow[1:5]
        obs <- mrow[-5:0]
        if(any(is.na(pars))) return(rep(NA,2101))
        tmodel <- prospect(pars,5)[,refltrans]
        tdiff <- tmodel - obs
        return(tdiff)
    }
    error.mat <- apply(rt.mat, 1, tdiff)
    return(error.mat)
}

#' With the `error.matrix` function defined, we apply it to reflectance and 
#' transmittance data. For each, we compute the error for all available data 
#' and again for hardwood and conifer species separately, due to the large 
#' systematic differences in both measurements and inversion parameter 
#' estimates for both. Here and consequently, we use the `re` prefix to 
#' indicate reflectance and `te` to indicate transmittance.

rem.all.raw <- error.matrix(fft.f, refl, 1)
rem.h.raw <- error.matrix(fft.f[plant.type=="hardwood"], refl, 1)
rem.c.raw <- error.matrix(fft.f[plant.type=="conifer"], refl, 1)
tem.all.raw <- error.matrix(fft.f, trans, 2)
tem.h.raw <- error.matrix(fft.f[plant.type=="hardwood"], trans, 2)
tem.c.raw <- error.matrix(fft.f[plant.type=="conifer"], trans, 2)

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
    err.mat <- t(err.mat)
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

em.list <- list(rem.all.raw, rem.h.raw, rem.c.raw,
                tem.all.raw, tem.h.raw, tem.c.raw)
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
        theme(text = element_text(size=25),
              axis.title = element_text(size=rel(1)),
            axis.text = element_text(size=rel(0.7)))
    return(err.spec)
}
#' Below, we apply our plot function to the data table we generated previously 
#' and save the resulting plot to a PDF.
dat.plot <- error.plot(dat.all, "measure~pft")
png("manuscript/figures/refltrans-validation.png", width=14, height=14, units="in", res=300, pointsize=25)
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
