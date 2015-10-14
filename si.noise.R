#' ---
#' title: Noise figure
#' author: Alexey Shiklomanov
#' output_format: pdf_document
#' ---

library(PEcAnRTM)
set.seed(777)
spec <- prospect(defparam("prospect_5"), 5)[,1]
noise <- generate.noise(n=2101, sigma=2.5e-4, fw=11)
obs <- spec + noise
wl <- 400:2500
wl.1 <- 525:575
ind.1 <- wl.1 - 399
wl.2 <- 1900:2000
ind.2 <- wl.2 - 399
png("manuscript/figures/SI.noise.png", height=5, width=5, units="in", res=300, pointsize=12)
par(mfrow=c(2,2), mar=c(4,4,2,2))
plot(wl.1, noise[ind.1], type='l', xlab="Wavelength", ylab="Noise")
plot(wl.2, noise[ind.2], type='l', xlab="Wavelength", ylab="Noise")
lines(wl, obs, col=2)
plot(wl.1, spec[ind.1], type='l', xlab="Wavelength", ylab="Reflectance")
lines(wl.1, obs[ind.1], col=2)
plot(wl.2, spec[ind.2], type='l', xlab="Wavelength", ylab="Reflectance")
lines(wl.2, obs[ind.2], col=2)
dev.off()
