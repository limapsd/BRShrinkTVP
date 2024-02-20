################################## LPDS Graph Script ######################################

#Load Packages
library("shrinkTVP")
library("dplyr")
library("gplots")
library("bvarsv")


library("RColorBrewer")
# Multicore LPDS
library("doParallel")
library("foreach")
# For manipulating dates
library("zoo")
# Load library for controlling number of BLAS threads
library("RhpcBLASctl")

# Define how many periods to calculate LPDS for
Tmax =  nrow(BRinf) - 1
T0   =  Tmax - 11

lpds = rbind(lpds_lasso,lpds_ng,lpds_lasso_sv,lpds_ng_sv)
rownames(lpds) = c("Hierachical Lasso","Hierachical Normal-Gamma","SV-LASSO","SV-NG") 


cumu_lpds = apply(lpds, 1, cumsum)
color = brewer.pal(4 ,"Set2")


# Plot results
par(mfrow=c(1,1))
colnames(cumu_lpds) = c("Hierachical Lasso","Hierachical Normal-Gamma","SV-LASSO","SV-NG")

matplot(cumu_lpds, type = "l", ylab = "Cumulative LPDS",
        xaxt = "n", xlab = "", col = color, lty=1, lwd=2 )

# Extract labels from the data frame

labs = as.yearmon(rownames(BRinf))[T0:Tmax + 1][c(FALSE, TRUE)]

# Create custom axis labels
axis(1, at = (1:length(T0:Tmax))[c(FALSE, TRUE)], labels = FALSE)
text(x=(1:length(T0:Tmax))[c(FALSE, TRUE)],
     y=par()$usr[3]-0.05*(par()$usr[4]-par()$usr[3]), srt=45, adj=1,labels= labs, xpd=TRUE)
# Add legend
legend("topright", colnames(cumu_lpds), col = color,
       lty = 1,lwd = 2, bty = "n", cex = 0.8)

