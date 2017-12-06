##### Description ####
' Make various correlation plots of the predictors '

#### Input ####
'MeteoMonth_demean <- read_csv("./data/data_processed/Meteo_train_demean.csv") <- " BaseData_Demean.R"'

#### Output ####
'
- Corplots of the entire predictor variables of the entire season, the summer months, those used in the model, the SMIs from May to Oct, 
  and PET and T for this period
- "figures/figures_exploratory/Train/CorrelationPlots/CorPlot_*"
'

#### Results ####
'
Schaut man sich die Correlationen an, dann gibt es folgende auffälligen Korrelationen
  - SMI ist stark autorkorreliert mit allen Vormonaten, wobei dies dann abnimmt. 
    - Correlation mit dem lag = 1 etwa 0.8
    - Correlation mit lag =2 zwischen 0.59 und 0.72
 - SMI stark korreliert mit Niederschlag
    - SMI des folgemonats ist sehr stark mit Niederschlag korreliert (über mehrer Lags abnehmend).
    - Niederschlag des Folgemonats ist aber wenig mit Niederschlag korreliert. 
    - 
  - SMI leicht korreliert mit Temperatur
    - in den meisten Monaten ist die Korrelation negative (May bis August)
    - im Oktober ist sie positiv
    - es gibt auch leichte autokorrelationen: so sind vor allem die Folgemonate der Bodenfeuchte mit Temperature abehmend negativ korreliert, 
      in die andere Richtung sind die Muster aber nicht so klar. 
    
  - Es gibt leichte Zusammenhänge Temperatur und Niederschlag
  - Variablen im Model 
      - SMI August korreliert sehr stark mit den anderen Variablen
        - positiv mit P_Jul und SMI_Aug, 
        - negativ mit T im July
      - T und P July korrelieren negativ - evtl kann ich P_July rauslassen

All correlations above threshold of 0.75 are found among the SMI Data and PET/T:
SMI_Jun and SMI_May - 0.8
SMI_Aug and SMI_Jul - 0.81 
SMI_Aug and SMI_Sep - 0.85 
SMI_Sep and SMI_Oct - 0.87
SMI_Sep and SMI_Oct. This means for any model selection approach, I only should rely on three of the SMIs.
Based on paper ones (BIC) I would recommend to use June, August, and October.

PET and T May: 0.87
PET and T June: 0.88
PET and T July: 0.94
PET and T Aug: 0.89
PET and T Sep: 0.78
PET and T Oct: 0.55



.


'

#### Packages #####
source("./script/script_raw/Packages.R")
#### Load Data ####
MeteoMonth_demean <- read_csv("./data/data_processed/Meteo_train_demean.csv")

MeteoMonth_demean 

#### Correllation Matrix of entire season ####
MeteoMonth_demean_corr_season <- MeteoMonth_demean %>% select(P_May_demeaned:P_Oct_demeaned, T_May_demeaned:T_Oct_demeaned, SMI_May:SMI_Oct)
MeteoMonth_demean_corr_season_PET <- MeteoMonth_demean %>% select(P_May_demeaned:P_Oct_demeaned, PET_May_demeaned:PET_Oct_demeaned, SMI_May:SMI_Oct)

corr_season <- round(cor(MeteoMonth_demean_corr_season), 2)
corr_season

corr_season_PET <- round(cor(MeteoMonth_demean_corr_season_PET), 2)
corr_season_PET

par(mfrow=c(2,2))
corrplot(corr_season, method="circle", type="upper")
corrplot(corr_season, method="number", type="upper")
corrplot(corr_season_PET, method="circle", type="upper")
corrplot(corr_season_PET, method="number", type="upper")

' The correlation plots show, that the patterns found for PET and T are very similiar. 
  Since PET delivers better fit based on cross validation mars models, I prefer PET instead of T. Thus, 
  I will work with PET instead of T.'



# pairs.panels(MeteoMonth_demean_corr_season )

highlyCorrelated <- findCorrelation(corr_season, cutoff=0.75, names=TRUE)
'
When using T:
All correlations above threshold of 0.75 are found among the SMI Data:
SMI_Jun and SMI_May
SMI_Aug and SMI_Jul
SMI_Sep and SMI_Oct. This means for any model selection approach, I only should rely on three of the SMIs.
Based on paper ones (BIC) I would recommend June, August, and October.'

highlyCorrelated_PET <- findCorrelation(corr_season_PET, cutoff=0.75, names=TRUE)

MeteoMonth_demean_corr_season <- MeteoMonth_demean_corr_season_PET 


#### Correllation Matrix of summer season ####
MeteoMonth_demean_corr_summer <- MeteoMonth_demean %>% select(P_Jun_demeaned:P_Aug_demeaned, PET_Jun_demeaned:PET_Aug_demeaned, SMI_Jun:SMI_Aug)
corr_summer <- round(cor(MeteoMonth_demean_corr_summer), 2)
corr_summer

par(mfrow=c(1,2))
corrplot(corr_summer, method="circle", type="upper")
corrplot(corr_summer, method="number", type="upper")

highlyCorrelated <- findCorrelation(corr_summer, cutoff=0.75, names=TRUE)


# pairs.panels(MeteoMonth_demean_corr_season )

#### Correlatino Matrix of Variables used ####
MeteoMonth_demean_corr_paper <- MeteoMonth_demean %>% select(P_Jul_demeaned, PET_Jul_demeaned, SMI_Jun,SMI_Aug)
corr_paper <- round(cor(MeteoMonth_demean_corr_paper), 2)
corr_paper

par(mfrow=c(1,2))
corrplot(corr_paper, method="circle", type="upper")
corrplot(corr_paper, method="number", type="upper")

highlyCorrelated <- findCorrelation(corr_paper, cutoff=0.75, names=TRUE)

#### Correlation Matrix of SMI ####
MeteoMonth_demean_corr_SMI <- MeteoMonth_demean %>% select(SMI_May:SMI_Oct)
corr_SMI <- round(cor(MeteoMonth_demean_corr_SMI), 2)
corr_SMI
summary(corr_SMI[upper.tri(corr_SMI)])

par(mfrow=c(1,2))
corrplot(corr_SMI, method="circle", type="upper")
corrplot(corr_SMI, method="number", type="upper")

highlyCorrelated <- findCorrelation(corr_SMI, cutoff=0.75, names=TRUE)
highlyCorrelated 

#### Correllation Matrix of PET and T ####
MeteoMonth_demean_corr_TPET <- MeteoMonth_demean %>% select(PET_May_demeaned:PET_Oct_demeaned, T_May_demeaned:T_Oct_demeaned)
corr_TPET <- round(cor(MeteoMonth_demean_corr_TPET), 2)
corr_TPET

par(mfrow=c(1,2))
corrplot(corr_TPET, method="circle", type="upper")
corrplot(corr_TPET, method="number", type="upper")
# pairs.panels(MeteoMonth_demean_corr_season )

highlyCorrelated <- findCorrelation(corr_TPET, cutoff=0.75, names=TRUE)

###############################################################################################################################################
#### Correlation Matrix Funktion derived from https://hlplab.wordpress.com/2012/03/20/correlation-plot-matrices-using-the-ellipse-library/ ####
###############################################################################################################################################

my.plotcorr <- function (corr, outline = FALSE, col = "grey", upper.panel = c("ellipse", "number", "none"), lower.panel = c("ellipse", "number", "none"), diag = c("none", "ellipse", "number"), digits = 2, bty = "n", axes = FALSE, xlab = "", ylab = "", asp = 1, cex.lab = par("cex.lab"), cex = 0.75 * par("cex"), mar = 0.1 + c(2, 2, 4, 2), ...)
{
  # this is a modified version of the plotcorr function from the ellipse package
  # this prints numbers and ellipses on the same plot but upper.panel and lower.panel changes what is displayed
  # diag now specifies what to put in the diagonal (numbers, ellipses, nothing)
  # digits specifies the number of digits after the . to round to
  # unlike the original, this function will always print x_i by x_i correlation rather than being able to drop it
  # modified by Esteban Buz
  if (!require('ellipse', quietly = TRUE, character = TRUE)) {
    stop("Need the ellipse library")
  }
  savepar <- par(pty = "s", mar = mar)
  on.exit(par(savepar))
  if (is.null(corr))
    return(invisible())
  if ((!is.matrix(corr)) || (round(min(corr, na.rm = TRUE), 6) < -1) || (round(max(corr, na.rm = TRUE), 6) > 1))
    stop("Need a correlation matrix")
  plot.new()
  par(new = TRUE)
  rowdim <- dim(corr)[1]
  coldim <- dim(corr)[2]
  rowlabs <- dimnames(corr)[[1]]
  collabs <- dimnames(corr)[[2]]
  if (is.null(rowlabs))
    rowlabs <- 1:rowdim
  if (is.null(collabs))
    collabs <- 1:coldim
  rowlabs <- as.character(rowlabs)
  collabs <- as.character(collabs)
  col <- rep(col, length = length(corr))
  dim(col) <- dim(corr)
  upper.panel <- match.arg(upper.panel)
  lower.panel <- match.arg(lower.panel)
  diag <- match.arg(diag)
  cols <- 1:coldim
  rows <- 1:rowdim
  maxdim <- max(length(rows), length(cols))
  plt <- par("plt")
  xlabwidth <- max(strwidth(rowlabs[rows], units = "figure", cex = cex.lab))/(plt[2] - plt[1])
  xlabwidth <- xlabwidth * maxdim/(1 - xlabwidth)
  ylabwidth <- max(strwidth(collabs[cols], units = "figure", cex = cex.lab))/(plt[4] - plt[3])
  ylabwidth <- ylabwidth * maxdim/(1 - ylabwidth)
  plot(c(-xlabwidth - 0.5, maxdim + 0.5), c(0.5, maxdim + 1 + ylabwidth), type = "n", bty = bty, axes = axes, xlab = "", ylab = "", asp = asp, cex.lab = cex.lab, ...)
  text(rep(0, length(rows)), length(rows):1, labels = rowlabs[rows], adj = 1, cex = cex.lab)
  text(cols, rep(length(rows) + 1, length(cols)), labels = collabs[cols], srt = 90, adj = 0, cex = cex.lab)
  mtext(xlab, 1, 0)
  mtext(ylab, 2, 0)
  mat <- diag(c(1, 1))
  plotcorrInternal <- function() {
    if (i == j){ #diag behavior
      if (diag == 'none'){
        return()
      } else if (diag == 'number'){
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else if (diag == 'ellipse') {
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      }
    } else if (i >= j){ #lower half of plot
      if (lower.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (lower.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    } else { #upper half of plot
      if (upper.panel == 'ellipse') { #check if ellipses should go here
        mat[1, 2] <- corr[i, j]
        mat[2, 1] <- mat[1, 2]
        ell <- ellipse(mat, t = 0.43)
        ell[, 1] <- ell[, 1] + j
        ell[, 2] <- ell[, 2] + length(rows) + 1 - i
        polygon(ell, col = col[i, j])
        if (outline)
          lines(ell)
      } else if (upper.panel == 'number') { #check if ellipses should go here
        text(j + 0.3, length(rows) + 1 - i, round(corr[i, j], digits=digits), adj = 1, cex = cex)
      } else {
        return()
      }
    }
  }
  for (i in 1:dim(corr)[1]) {
    for (j in 1:dim(corr)[2]) {
      plotcorrInternal()
    }
  }
  invisible()
}

##########################################
#### Define ColorRamp for my.plotcorr ####
# colsc=c("darkred", 'white', "blue4")
colsc=c(rgb(241, 54, 23, maxColorValue=255), 'white', rgb(0, 61, 104, maxColorValue=255))

# Build a ramp function to interpolate along the scale, I've opted for the Lab interpolation rather than the default rgb, check the documentation about the differences
colramp = colorRampPalette(colsc)

# Allow one hundred steps between blue and red ###
colors = colramp(100)

###################################
#### Plot of data use in paper ####
par(mfrow=c(1,1))
my.plotcorr (corr_paper,  col=colors[((corr_paper+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')

## Export PDF ##
pdf("figures/figures_exploratory/Train/CorrelationPlots/CorPlot_paper", width = 12, height = 12)
my.plotcorr (corr_paper,  col=colors[((corr_paper+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
dev.off()


########################
#### Plot of season ####
par(mfrow=c(1,1))
my.plotcorr (corr_season,  col=colors[((corr_season+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
my.plotcorr (corr_season_PET,  col=colors[((corr_season+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')


## Export PDF ##
pdf("figures/figures_exploratory/Train/CorrelationPlots/CorPlot_season", width = 12, height = 12)
my.plotcorr (corr_season,  col=colors[((corr_season+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
dev.off()

## Export PDF - PET ###
pdf("figures/figures_exploratory/Train/CorrelationPlots/CorPlot_season_PET", width = 12, height = 12)
my.plotcorr (corr_season_PET,  col=colors[((corr_season+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
dev.off()


#########################
#### Plot of sommer ####
par(mfrow=c(1,1))
my.plotcorr (corr_summer,  col=colors[((corr_summer+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')

## Export PDF ##
pdf("figures/figures_exploratory/Train/CorrelationPlots/CorPlot_summer", width = 12, height = 12)
my.plotcorr (corr_summer,  col=colors[((corr_summer+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
dev.off()

######################
#### Plot of SMIs ####
par(mfrow=c(1,1))
my.plotcorr (corr_SMI,  col=colors[((corr_SMI + 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')

## Export PDF ##
pdf("figures/figures_exploratory/Train/CorrelationPlots/CorPlot_SMI", width = 12, height = 12)
my.plotcorr (corr_SMI,  col=colors[((corr_summer+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
dev.off()

######################
#### Plot of T and PET ####
par(mfrow=c(1,1))
my.plotcorr (corr_TPET,  col=colors[((corr_TPET + 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')

## Export PDF ##
pdf("figures/figures_exploratory/Train/CorrelationPlots/CorPlot_TPET", width = 12, height = 12)
my.plotcorr (corr_TPET,  col=colors[((corr_summer+ 1)/2) * 100], diag='ellipse', lower.panel="number", main='Predictor correlations')
dev.off()




