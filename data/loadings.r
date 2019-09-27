
## Installation of packages
install.packages(c("lme4","lmerTest","lsmeans","tidyverse","vegan","indicspecies","reshape2","mgcv"))

## load main articles
library(tidyverse)
library(reshape2)

## Load functions
## Standard error
se <- function(x) {sd(x)/sqrt(length(x))}


## RII
rii.fun <- function(data, compare, treat, control){

  ## describe variables
  dataset <- data
  shrub <- enquo(treat)
  open <- enquo(control)

  ## Determine the columns that are site identifiers (e.g. rep, site name, year) and the columns that RII should apply to - i.e. all numeric values
  site <- dataset[!unlist(lapply(dataset, is.numeric))] ## Site values
  nums <- dataset[unlist(lapply(dataset, is.numeric))] ## numeric values for RII
  site.nocompare <- site[,!names(site)==compare]   ## drop column for comparison

  ## First generate a column with unique identfiers for the dataset
  site.name <- names(site.nocompare) ## site names
  nums.name <- names(nums) ## column names

  no.compare <- unique(site.nocompare) ## find unique instances
  no.compare[,"uniqueID"] <- seq(1,nrow(no.compare),1) ## add unique identifier column
  unique.site <- merge(dataset, no.compare, by=c(site.name)) ## join unique identifier back to original dataset



  rii <- function(x){
    f1 <- as.formula(paste("uniqueID", "~", compare))## formula for dcast
    unique.site %>% data.table::dcast(data=., f1, mean, value.var=x) %>% mutate(RII=(!!shrub-!!open)/(!!shrub+!!open)) %>%  select(RII)
  }

  rii.data <- data.frame(lapply(nums.name, rii))
  colnames(rii.data) <- nums.name
  rii.data <- cbind(site.nocompare[!duplicated(site.nocompare), ],rii.data)
  rii.data[is.na(rii.data)] <- 0 ## convert NA to zeros

  return(rii.data)
}

## load data
## Drug trial data
drug.trial <- data.frame(response = c(1,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1,0,0,0,1,0,0,1,1,1,1,1,0,0,1,0,1,1,1,0,1,0,0,0,1,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1),
predictor = c(rep("control",20),rep("drug1",20),rep("drug2",20),rep("drug3",20)))

## Workshop practice
bird.data <- data.frame(species.rich =c(rpois(50,5),rpois(50,3),rpois(50,3.1)),habitat = c(rep("forest",50),rep("urban.parks",50),rep("city",50)))

## Data for GLMM
set.seed(2)
ranch <- data.frame(cattle=c(20,18,19,18,17,19,15,13,14,12,14,11,10,9,11,8,9,10,7,5),biomass=sort(exp(rnorm(20))), year=rep(c(2014,2015,2016,2017),5))

## Data for GAM
gam.data<- data.frame(year = c(1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010),
ndvi =c(1.53,1.57,1.53,1.85,1.84,1.85,2.15,2.13,2.46,2.13,2.46,2.42,2.3,2.56,2.15,2.7,3.07,2.7,3.07,2.56,2.46,2.28,2.3,1.85,2.15,1.99,1.84,1.85,2.3,2.28,2.61))


## load extra datasets
grazing <- read.table("https://raw.githubusercontent.com/afilazzola/UoA.CommunityAnalyses.2018/master/data/grazing.txt")
multivar <- read.table("https://raw.githubusercontent.com/afilazzola/UoA.CommunityAnalyses.2018/master/data/multivar.txt")
practice <- read.table("https://raw.githubusercontent.com/afilazzola/UoA.CommunityAnalyses.2018/master/data/practice.txt")
samplelong <- read.table("https://raw.githubusercontent.com/afilazzola/UoA.CommunityAnalyses.2018/master/data/samplelong.txt")