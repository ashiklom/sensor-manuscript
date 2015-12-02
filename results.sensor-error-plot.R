#' ---
#' title: Sensor error plot
#' author: Alexey Shiklomanov
#' output_format: html_document
#' ---

#' # Introduction
#' This script generates a large matrix of scatterplots that show the ability 
#' of each sensor to accurately retrieve parameters from the simulations. 

#' # Setup
#' First, we load dependencies and data. The PEcAnRTM package is used only for 
#' pre-defined sensor information. The remaining packages are used to generate 
#' the plots. For details on how the data file is generated, see the 
#' `load.sim.R` file. 

library(PEcAnRTM)
library(ggplot2)
library(gridExtra)
library(grid)
load("data/simulation.results.RData")

#' To facilitate plot organization, we first change the sensor names to their 
#' pretty versions (`sensor.proper`) and then convert the representation of the 
#' sensor from character to factor with a pre-defined order based on spectral 
#' resolution.
simulation.dat[, sensor := sensor.proper[sensor]]
simulation.dat[, sensor := factor(sensor, levels=sensor.proper)]

#' # Plot specifications
#' The following lines provide graphical preferences for the plot. The `no.x` 
#' theme is an additional set of parameters that removes x axis labels. The 
#' `gen.plot` object is a generalized `ggplot` object describing the data's 
#' presentation, but without any data present (subsequent lines add this data 
#' in turn for each parameter). For each parameter (rows) and sensor (columns, 
#' in order of decreasing spectral resolution), this script draws one 
#' scatterplot of true parameter values (x) vs mean inversion estimates (y) 
#' with a dashed red one-to-one line. Specifically, each parameter (row) is 
#' created as a separate plot, and these plots are then stacked on top of each 
#' other using the `grid.arrange` function.


sensor.error.plot <- function(gen.theme){
    no.x <- theme(axis.title.x = element_blank())
    gen.plot <- ggplot(simulation.dat) + 
        facet_grid(.~sensor) + geom_point(size=2) +
            geom_abline(linetype="dashed", color="red", size=1.5) +
            gen.theme
        N.plot <- gen.plot + aes(x=N, y=N.mu) + ylab("N") + no.x
            #scale_x_continuous(breaks=c(1, 1.6, 2.2, 2.8, 3.4))
        Cab.plot <- gen.plot + aes(x=Cab, y=Cab.mu) + ylab("Cab") + no.x
        Car.plot <- gen.plot + aes(x=Car, y=Car.mu) + ylab("Car") + no.x
        Cw.plot <- gen.plot + aes(x=Cw, y=Cw.mu) + ylab("Cw") + no.x +
            scale_x_continuous(breaks=c(0, 150, 300, 450))
        Cm.plot <- gen.plot + aes(x=Cm, y=Cm.mu) + ylab("Cm") + no.x +
            scale_x_continuous(breaks=c(100, 200, 300))
        grid.arrange(N.plot, Cab.plot, Car.plot, Cw.plot, Cm.plot, ncol=1)
}
theme.poster <- theme_bw() + 
    theme(axis.text = element_text(size=9),
          strip.text = element_text(size=11),
          axis.title.y = element_text(size=28),
          plot.margin = unit(c(0.2, 0.2, 0, 0), "lines"))
png("figures/sensor-bias.poster.png", height=14, width=14, units="in", res=300)
sensor.error.plot(theme.poster)
dev.off()
theme.paper <- theme_bw() + 
    theme(axis.text = element_text(size=6),
          strip.text = element_text(size=6),
          axis.title.y = element_text(size=14),
          plot.margin = unit(c(0.2, 0.2, 0, 0), "lines"))
png("figures/sensor-bias.paper.png", height=9, width=7.5, units="in", res=300)
sensor.error.plot(theme.paper)
dev.off()

#' Here, we explore a few alternative ways of plotting the results of the 
#' sensor simulation experiment. The figure from this section that ended up in 
#' the supplementary information shows the relative width of the 95% confidence 
#' interval of the inversion parameters relative to the mean as a function of 
#' the true parameter value.  To facilitate generation and customization of 
#' this figure, we take advantage of the `sprintf` function for string 
#' processing combined with the `aes_string` feature from ggplot to create a 
#' common plotting function that we then `mapply` over the list of parameters 
#' and use `do.call` to arrange the resulting list of plots.

gp2 <- ggplot(simulation.dat) + facet_grid(.~sensor) + geom_point(size=1) + theme.paper
plt.error <- function(param, string, parname){
    lab <- sprintf("pi(%s)", parname)
    plt <- gp2 + aes_string(x=param, y=sprintf(string, param)) + no.x +
        ylab(parse(text=lab))
    return(plt)
}

# Plot of relative error vs. true parameter value
#cv.str <- "%1$s.mu/%1$s - 1"
#cv.list <- c(lapply(params.prospect5, plt.error, string=cv.str), ncol=1)
#do.call(grid.arrange, cv.list)

riqr.str <- "(%1$s.q975 - %1$s.q25)/%1$s.mu"
riqr.list <- c(mapply(plt.error, params.prospect5, riqr.str, params.prospect5, SIMPLIFY=FALSE), 
               ncol=1)
png("figures/sensor-riqr.paper.png", height=9, width=7.5, units="in", res=300)
do.call(grid.arrange, riqr.list)
dev.off()

