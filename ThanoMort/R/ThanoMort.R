library(reshape2)
library(devtools)
library(data.table)
# install 3 packages from github, with the commented out code. Requires
# newish devtools version, plus you need package-building powers on your
# system, which is default in linux and needs to be configures in Mac or
# Windows (Windows is the hardest).
# when installing, you may get some inexplicable warnings like:
# Warning message:
# package ‘’ is not available (for R version 3.1.0)
# just ignore that, I think maybe it's a devtools thing, but there may be some minor glitch
# in my installation thing. Haven't figured it out yet.
# install_github("DemogBerkeley", subdir = "DemogBerkeley", username = "UCBdemography")
# install_github("LexisUtils", subdir = "LexisUtils", username = "timriffe")
# install_github(repo = "Leaves", subdir = "PlosOne/R/RiffeetalFunctions", username = "timriffe")

library(RiffeetalFunctions)
library(LexisUtils)
library(DemogBerkeley)

# read HMD data live from web.
# pw <- userInput()
# us <- userInput()
# function adds some value by coercing columns to useable classes.
LT <- readHMDweb("USA","mltper_1x1",password=pw,username=us)

# 0) scale to radix of 1. Make life easier.
LT <- data.table(LT)
standardize <- function(SD){
    SD$lx <- SD$lx / 1e5
    SD$Lx <- SD$Lx / 1e5
    SD$dx <- SD$dx / sum(SD$dx) # due to rounding, need to scale like this.
                                # is slightly misaligns dx and lx in some cases
    SD
}
# exotic data.table syntax, works like a charm
LT <- LT[, standardize(.SD), by = list(Year)]

getmy <- function(mx,dx){
    rowSums(Thano(mx,dx))
}

# 1) what does mx look like before and after transformation?
LT[,my := getmy(mx,dx), by = list(Year)]

# look at one year of m(y)
yrind <- LT$Year == 2010
pdf("/home/triffe/workspace/ThanoMort/Figures/my.pdf")
plot(0:110, LT$my[yrind], type = 'l', log = 'y', xlab = "Years Left", ylab = "ln(m(y))")
dev.off()

# look at surface of m(y,t)
MY <- acast(LT, Age~Year, value.var = "my")
#matplot(MY,type='l',lty=1,log='y') # as lines (all variation in middle)
MY[MY == 0] <- NA

pdf("/home/triffe/workspace/ThanoMort/Figures/MY.pdf")
LexisMap(MY,contour=TRUE,ylab="Years Left", main = "m(x) transformed thanatologically\nUS males")
dev.off()

# 2) sanity-check. lx should equal ly when x = y
plot(LT$lx[yrind],rowSums(Thano(LT$lx[yrind],LT$dx[yrind],FALSE)))
# I'm satisfied!

# 3) I think nr 1 doesn't work because as the lifetable stretches to age infinity
# we just keep summing in more mx material and it piles on age 0. There's no good 
# way to close it out! So what does it look like to transform dx itself?
DY <- Thano(LT$dx[yrind],LT$dx[yrind])
DY[,ncol(DY):1][lower.tri(DY)] <- NA
pdf("/home/triffe/workspace/ThanoMort/Figures/dy.pdf")
plot(0:110,rowSums(DY, na.rm=TRUE),type='l',ylab="d**(y)",xlab="Years Left")
lines(0:110,LT$dx[yrind],col="green")
text(80,.04,"d(x), for reference")
dev.off()
# graphics.off()
# this is what the surface looks like, in case that helps
LexisMap(DY,log=FALSE)
#LexisMap(DY,log=FALSE,contour=TRUE)

# again, logged
pdf("/home/triffe/workspace/ThanoMort/Figures/lndy.pdf")
plot(0:110,rowSums(DY, na.rm=TRUE),type='l',ylab="d**(y)",xlab="Years Left",log="y")
lines(0:110,LT$dx[yrind],col="green")
text(80,.04,"d(x), for reference")
dev.off()