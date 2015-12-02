#' ---
#' title: Sensor inaccuracy and uncertainty table
#' author: Alexey Shiklomanov
#' output_format: html_document
#' ---

#' # Introduction
#' This script is used to generate the summary table for the sensor simulation 
#' experiment. For each parameter and sensor, the table shows on average how 
#' close the mean inversion estimate was to the true value (inaccuracy) and the 
#' average uncertainty (expressed as relative standard deviation) of the 
#' inversion estimate.

#' # Setup
#' First, we load dependencies and data. `PEcAnRTM` is loaded for parameter 
#' naming and for its implicit use of the `data.table` package. For information 
#' on how `simulation.results.RData` is generated, see the `load.sim.R` 
#' script.

library(PEcAnRTM)
load("data/simulation.results.RData")

#' To facilitate ordering of the table, we convert the representation of the 
#' sensor in the data table (NOTE: `simulation.dat` is a `data.table` object, 
#' NOT a `data.frame`, hence the different syntax) from character to factor. In 
#' addition, we exclude any values that do not have a sensor designation.

simulation.dat[, sensor := factor(sensor, levels=sensor.list)]
simulation.dat <- simulation.dat[!is.na(sensor)]

#' # Calculate statistics
#' This block of code computes the relevant statistics for each sensor. `rsd` 
#' is a measure of parameter uncertainty, defined as the width of the 95% 
#' confidence interval divided by the mean. `cv` is the error in the mean value 
#' of the inversion estimate relative to the true value, which we use as the 
#' measure of inaccuracy. We use the `data.table` column definition syntax (via 
#' the `:=` operator) to perform this efficiently.

simulation.dat[, N.pi := 100 * (N.q975 - N.q25)/ N.mu]
simulation.dat[, Cab.pi := 100 * (Cab.q975 - Cab.q25)/ Cab.mu]
simulation.dat[, Car.pi := 100 * (Car.q975 - Car.q25)/ Car.mu]
simulation.dat[, Cw.pi := 100 * (Cw.q975 - Cw.q25)/ Cw.mu]
simulation.dat[, Cm.pi := 100 * (Cm.q975 - Cm.q25)/ Cm.mu]
simulation.dat[, N.alpha := 100 * (N.mu - N)/N]
simulation.dat[, Cab.alpha := 100 * (Cab.mu - Cab)/Cab]
simulation.dat[, Car.alpha := 100 * (Car.mu - Car)/Car]
simulation.dat[, Cw.alpha := 100 * (Cw.mu - Cw)/Cw]
simulation.dat[, Cm.alpha := 100 * (Cm.mu - Cm)/Cm]

#' Next, we use `data.table` aggregation syntax to compute the mean values by 
#' sensor of each of the above statistics for each parameter.

rnames <- sprintf("%s.pi", params.prospect5)
cnames <- sprintf("%s.alpha", params.prospect5)
sensor.table <- simulation.dat[, lapply(.SD, mean, na.rm=TRUE), by=sensor, 
                        .SDcols=c(rnames, cnames)]
setkey(sensor.table, sensor)
sensor.table <- sensor.table[sensor.list]
sensor.table[, sensor := sensor.proper[sensor]]
print(sensor.table, digits=3)

#' We now plot the statistics. 
cols <- c("darkred", "red", "orange", "yellow", "green", 
          "darkgreen", "blue", "darkblue", "turquoise", 
          "darkviolet", "grey")
names(cols) <- sensor.proper

pdf("figures/uncertainty.pdf")
gp <- par(mfrow=c(2,3), mar=c(2,0,2,0), oma=c(3,4.5,2,2), 
          cex=1, cex.axis=0.8)
y1 <- c(0,120)
y2 <- c(0,250)
with(sensor.table, {
         barplot(N.pi, col=cols, main="N", ylim=y1)
         abline(h=100, lty=2)
         barplot(Cab.pi, col=cols, main="Cab", ylim=y1, yaxt='n')
         abline(h=100, lty=2)
         barplot(Cw.pi, col=cols, main="Cw", ylim=y1, yaxt='n')
         abline(h=100, lty=2)
         barplot(Car.pi, col=cols, main="Car", ylim=y2)
         abline(h=100, lty=2)
         barplot(Cm.pi, col=cols, main="Cm", ylim=y2, yaxt='n')
         abline(h=100, lty=2)
         })
mtext("Mean relative uncertainty (%)", 2, outer=TRUE, line=3)
par(gp)
leg <- par(usr=c(0,1,0,1), xpd=NA, cex=1)
legend(x=0.7, y=0.46, legend=sensor.proper, fill=cols)#, lty=1, lwd=4)
dev.off()


pdf("figures/relbias.pdf")
gp <- par(mfrow=c(2,3), mar=c(1,0,2,0), oma=c(3,4.5,0,1), 
          cex=1, cex.axis=0.8)
y1 <- c(-2,2)
y2 <- c(-40, 0)
y3 <- c(0, 10)
with(sensor.table, {
         barplot(N.alpha, col=cols, main="N", ylim=y1)
         abline(h=100, lty=2)
         Cab.avhrr <- Cab.alpha[11]
         Cab.alpha[11] <- -1.5
         barplot(Cab.alpha, col=cols, main="Cab", ylim=y1, yaxt='n')
         abline(h=100, lty=2)
         text(x=12.7, y=-2, labels=sprintf("%.1f", Cab.avhrr), cex=0.6, xpd=NA)
         arrows(x0=12.7, y0=-1.54, y1=-1.86, col="grey", lwd=2, length=0.05)
         Cw.avhrr <- Cw.alpha[11]
         Cw.alpha[11] <- -1.5
         barplot(Cw.alpha, col=cols, main="Cw", ylim=y1, yaxt='n')
         abline(h=100, lty=2)
         text(x=12.7, y=-2, labels=sprintf("%.1f", Cw.avhrr), cex=0.6, xpd=NA)
         arrows(x0=12.7, y0=-1.54, y1=-1.86, col="grey", lwd=2, length=0.05)
         barplot(Car.alpha, col=cols, main="Car", ylim=y2)
         abline(h=100, lty=2)
         plot.new()
         Cm.alpha.trim <- Cm.alpha
         Cm.alpha.trim[Cm.alpha > 10] <- 8.5
         Cm.alpha.big <- Cm.alpha[Cm.alpha > 10]
         big <- sensor.proper[Cm.alpha > 10]
         barplot(Cm.alpha.trim, col=cols, main="Cm", ylim=y3)
         text(x=c(5.5,12.7), y=9.8, labels=sprintf("%.1f", Cm.alpha.big), cex=0.6)
         arrows(x0=c(5.5,12.7), y0=8.6, y1=9.5, col=cols[big], lwd=2,
                length=0.05)
         abline(h=100, lty=2)
         })
mtext("Mean relative bias (%)", 2, outer=TRUE, line=3)
dev.off()
#par(gp)
#leg <- par(usr=c(0,1,0,1), xpd=NA, cex=0.6)
#legend(x=0.37, y=0.46, legend=sensor.proper, fill=cols)#, lty=1, lwd=4)

#' # Table formatting
#' Rather than using ugly R variable names, we add more informative row and 
#' column labels to the table in preparation for export. `sensor.list` and 
#' `sensor.proper` are cross-referenced character vectors defined by PEcAnRTM 
#' containing the names of sensors as used by R (`sensor.list`) and properly 
#' formatted for publication (`sensor.proper`). 

setcolorder(sensor.table, c("sensor", rnames, cnames))
setnames(sensor.table, "sensor", "Sensor")
setnames(sensor.table, cnames, sprintf("$\\alpha(\\mathrm{%s})$", params.prospect5))
setnames(sensor.table, rnames, sprintf("$\\pi(\\mathrm{%s})$", params.prospect5))

#' We use xtable to generate a LaTeX version of the table, specifying the 
#' caption, number of significant figures (`digits`), and TeX reference 
#' (`label`). We call the print statement with no file argument to store the 
#' result as a string. The `sanitize.text.function` defines how xtable deals 
#' with escape characters such as `\`; we set this function to identity to use 
#' exactly the text we gave because our column names already have TeX 
#' formatting. Finally, we post-process the resulting raw string to add the 
#' `centerline` option, which centers the table across the entire page rather 
#' than strictly conforming to the left margin as per the default behavior. The 
#' table is written to a `.tex` file in the `manuscript/tables` directory.

cap <- "
Mean uncertainty ($\\pi$) and inaccuracy ($\\alpha$) by sensor, expressed as percentage (x 100).
"
cap <- gsub("\\n", " ", cap)
out.tab <- xtable(sensor.table, caption=cap, digits=3, label="tab:sensor")
out.tab.pre <- print(out.tab, file="", include.rownames=FALSE,
                     sanitize.text.function = function(x) x)
out.tab.post <- out.tab.pre
out.tab.post <- gsub("centering", "centerline{", out.tab.post)
out.tab.post <- gsub("(end\\{tabular\\})", "\\1\n\\}", out.tab.post)
cat(out.tab.post, file="manuscript/tables/tab-sensor.tex")
