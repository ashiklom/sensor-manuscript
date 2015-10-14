#' ---
#' title: SI Prior figure
#' author: Alexey Shiklomanov
#' output_format: html_document
#' ---

#' # Introduction
#' This script generates a figure showing the PDFs of the priors used for the 
#' Bayesian inversion.

#' # Setup
#' First, we load dependencies. PEcAnRTM defines the priors and parameters. The 
#' remaining packages are used for plotting.

library(PEcAnRTM)
library(ggplot2)
library(grid)
library(gridExtra)

#' Next, we set up the inputs. We plot the PDF for each parameter over the 
#' entirety of its reasonable range, given by the following vectors (`xs` is 
#' used as a shortcut function for generating a sequence).

xs <- function(a, b) seq(a, b, length.out=100)
N.x <- xs(1, 5)
Cab.x <- xs(0, 100)
Car.x <- xs(0, 40)
Cw.x <- xs(0, 0.05)
Cm.x <- xs(0, 0.04)
xx <- data.frame(N.x, Cab.x, Car.x, Cw.x, Cm.x)

#' # Calculating the PDF
#' First, we load the PROSPECT5 priors from the PEcAnRTM package.

priors <- prior.defaultvals.prospect()
pmu <- priors$mu
psd <- priors$sigma

#' Then, we set up a custom function for the PDF of N, which is an offset 
#' lognormal distribution.
dlnorm.N <- function(x, mu, sigma) dlnorm(x-1, mu, sigma)

#' We set up the plot theme and labels.
th.prior <- theme_bw() + 
    theme(text = element_text(size=8),
          axis.text = element_text(size=rel(0.6)),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "lines"))
th.noY <- theme(axis.title.y = element_blank())
lab.N <- xlab("N")
lab.Cab <- xlab(expression("Cab ("*mu*"g cm"^-2*")"))
lab.Car <- xlab(expression("Car ("*mu*"g cm"^-2*")"))
lab.Cw <- xlab("Cw (cm)")
lab.Cm <- xlab(expression("Cm (g cm"^-2*")"))

#' We then generate the plots, implicitly performing the calculations using 
#' ggplot's `stat_function` function.

pN <- ggplot(xx) + aes(x=N.x) + xlab("N") + ylab("Density") +
    stat_function(fun=dlnorm.N, arg=list(pmu["N"], psd["N"])) + th.prior
pCab <- ggplot(xx) + aes(x=Cab.x) + lab.Cab +
    stat_function(fun=dlnorm, arg=list(pmu["Cab"], psd["Cab"])) + th.prior + th.noY
pCar <- ggplot(xx) + aes(x=Car.x) + lab.Car +
    stat_function(fun=dlnorm, arg=list(pmu["Car"], psd["Car"])) + th.prior + th.noY
pCw <- ggplot(xx) + aes(x=Cw.x) + lab.Cw + ylab("Density") +
    stat_function(fun=dlnorm, arg=list(pmu["Cw"], psd["Cw"])) + th.prior
pCm <- ggplot(xx) + aes(x=Cm.x) + lab.Cm + th.noY +
    stat_function(fun=dlnorm, arg=list(pmu["Cm"], psd["Cm"])) + th.prior + th.noY

png("manuscript/figures/priors.png", height=3, width=4, units="in", res=300)
grid.arrange(pN, pCab, pCar, pCw, pCm, nrow=2)
dev.off()
