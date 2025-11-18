# load necessary packages
library("metafor")
library("tidyverse")
library("ggplot2")
library("dplyr")

### TEST DATASET WITH ALL SOURCES
# read in dataset
df_Liu <- read.csv("McPeake_MetaAnalysis_Liu.csv")

# calculate log response ratios and variances
df_Liu <- escalc(measure="ROM", m1i=meanStart, m2i=meanControl, sd1i=STDStart, 
             sd2i=STDControl, n1i=nStart, n2i=nControl, data=df_Liu)

# estimating the mean response ratio and variance
overallResult_Liu <- rma(yi, vi, data=df_Liu)
overallResult_Liu

# create a forest plot
forest.rma(overallResult_Liu, slab=df_Liu$Author, header="Author")

# check for outliers using infleunce analysis
influenceResults_Liu <- influence(overallResult_Liu)
influenceResults_Liu

# moderator test
mod.wetlandTypeq_Liu <- rma(yi, vi, mods = ~Wetland_Comparison-1, data=df_Liu)
mod.wetlandTypeq_Liu

# random effect structure
# mod.RESq <- rma.mv(yi, vi, random = ~1|Author, data=df)
# mod.RESq

# Funnel plots for publication bias
funnel(overallResult_Liu)

### TEST DATASET WITHOUT LIU ET AL.
# read in dataset
df_noLiu <- read.csv("McPeake_MetaAnalysis_NoLiu.csv")

# calculate log response ratios and variances
df_noLiu <- escalc(measure="ROM", m1i=meanStart, m2i=meanControl, sd1i=STDStart, 
                   sd2i=STDControl, n1i=nStart, n2i=nControl, data=df_noLiu)

# estimating the mean response ratio and variance
overallResult_noLiu <- rma(yi, vi, data=df_noLiu)
overallResult_noLiu

# create a forest plot
forest.rma(overallResult_noLiu, slab=df_noLiu$Author, header="Author")

# check for outliers using infleunce analysis
influenceResults_noLiu <- influence(overallResult_noLiu)
influenceResults_noLiu

# moderator test
mod.wetlandTypeq_noLiu <- rma(yi, vi, mods = ~Wetland_Comparison-1, data=df_noLiu)
mod.wetlandTypeq_noLiu

# random effect structure
# mod.RESq <- rma.mv(yi, vi, random = ~1|Author, data=df)
# mod.RESq

# Funnel plots for publication bias
funnel(overallResult_noLiu)
