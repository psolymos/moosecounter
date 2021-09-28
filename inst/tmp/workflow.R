#remotes::install_github("psolymos/moosecounter")
library(moosecounter)

## modify options as needed
mc_options(B=20)

## load/read data set
x <- read.csv("inst/extdata/MayoMMU_QuerriedData.csv")

## set response in options
#switch_response("cows")
switch_response("total")

## define surveyed units and take a subset as needed
## this also sets total cows value
x <- mc_update_total(x)

## univariate exploration
mc_plot_univariate("Subalp_Shrub_250buf", x, "ZINB")

## multivariate exploration
vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")

mc_plot_multivariate(vars, x)

## build model list
ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", weighted=TRUE)
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", weighted=TRUE)
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", weighted=TRUE)
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", weighted=TRUE)

mc_models_total(ML, x)
mc_plot_residuals("Model 3", ML, x)

## PI calculation
PI <- mc_predict_total(
    model_id=c("Model 1", "Model 3"),
    ml=ML,
    x=x,
    do_boot=TRUE, do_avg=TRUE)

mc_get_pred(PI)
pred_density_moose_PI(PI)
mc_plot_predpi(PI)
mc_plot_pidistr(PI)
mc_plot_pidistr(PI, id=1)

# shinyBS shinydashboard
run_app()

shiny::runApp("inst/shiny")

## PI

library(moosecounter)
mc_options(B=20)
x <- read.csv("inst/extdata/MayoMMU_QuerriedData.csv")
switch_response("total")
x <- mc_update_total(x)
vars <- c("ELC_Subalpine", "Fire1982_2012", "Fire8212_DEM815",
    "NALC_Needle", "NALC_Shrub", "Subalp_Shrub_250buf",
    "ELCSub_Fire8212DEM815", "SubShrub250_Fire8212DEM815")
ML <- list()
ML[["Model 0"]] <- mc_fit_total(x, dist="ZINB", weighted=TRUE)
ML[["Model 1"]] <- mc_fit_total(x, vars[1:2], dist="ZINB", weighted=TRUE)
ML[["Model 2"]] <- mc_fit_total(x, vars[2:3], dist="ZIP", weighted=TRUE)
ML[["Model 3"]] <- mc_fit_total(x, vars[3:4], dist="ZINB", weighted=TRUE)
PI <- mc_predict_total(
    model_id=c("Model 1", "Model 3"),
    ml=ML,
    x=x,
    do_boot=TRUE, do_avg=TRUE)

head(mc_get_pred(PI)$data)

pred_density_moose_PI(PI)
mc_plot_predpi(PI)
mc_plot_pidistr(PI)
mc_plot_pidistr(PI, id=2)


## --
set.seed(1)
data1 <- read.csv(file.path("~/repos/DeducerPlugInMoose/inst/extdata",
    "UKH_2017_QuerriedSU_Final_Corrected_for_Analysis.csv"))
moose_options(B=10)

switch_response('cows')

moose_options(srv="Sampled==1")
moose_options(MAXCELL=NULL)
checkModelList()
saveMooseData(data1, data1$Sampled==1)
ModelList[['TK_AlNeedWet_w']] <- wzi(
    zeroinfl(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='negbin', link='logit'))
summary(ModelList[['TK_AlNeedWet_w']])
plotResiduals('TK_AlNeedWet_w')
print(updateModelTab())
summary(ModelList[['TK_AlNeedWet_w']])
PI <- MooseSim.PI(c('TK_AlNeedWet_w'), do_avg=FALSE)
savePiData(PI)
plot_predPI(PI)
pred_density_moose_PI(PI)

checkCompModelList()
checkCompData()
CompModelList[['FireDEMSub']] <- fitCompModel(~ Fire8212_DEM815_Sub)
CPI <- MooseCompSimMMU.PI1(c('TK_AlNeedWet_w'), 'FireDEMSub', do_avg=FALSE)
saveCpiData(CPI)
pred_density_moose_CPI(CPI)

## dual prediction

library(DeducerPlugInMoose)
pboptions(type="timer")
set.seed(1)
data1 <- read.csv(file.path("~/repos/DeducerPlugInMoose/inst/extdata",
    "UKH2017_MMU_Querried_data_for_Analysis_Final_3.csv"))
moose_options(B=10)
moose_options(srv="Sampled==1")
moose_options(area_srv="In1Out0==1")
moose_options(MAXCELL=NULL)
checkModelList()
saveMooseData(data1, data1$Sampled==1)
ModelList[['TK_AlNeedWet_w']] <- wzi(
    zeroinfl(MOOSE_TOTA ~ LKStrat_01 + AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='negbin', link='logit'))
summary(ModelList[['TK_AlNeedWet_w']])
plotResiduals('TK_AlNeedWet_w')
print(updateModelTab())
summary(ModelList[['TK_AlNeedWet_w']])

PI <- MooseSim.PI(c('TK_AlNeedWet_w'), do_avg=FALSE)
savePiData(PI)
plot_predPI(PI)
pred_density_moose_PI(PI)

ModelList[['TK_AlNeedWet_w']] <- wzi(
    zeroinfl(MOOSE_TOTA ~ LKStrat_01 + AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='negbin', link='logit'))
checkCompModelList()
checkCompData()
CompModelList[['FireDEMSub']] <- fitCompModel(~ Fire8212_DEM800to1200)
CPI <- MooseCompSimMMU.PI1(c('TK_AlNeedWet_w'), 'FireDEMSub', do_avg=FALSE)
saveCpiData(CPI)
pred_density_moose_CPI(CPI, digits=3)

ModelList[['TK_AlNeedWet_w']] <-
    zeroinfl(MOOSE_TOTA ~ LKStrat_01 + AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='negbin', link='logit')
checkCompModelList()
checkCompData()
CompModelList[['FireDEMSub']] <- fitCompModel(~ Fire8212_DEM800to1200)
CPI <- MooseCompSimMMU.PI1(c('TK_AlNeedWet_w'), 'FireDEMSub', do_avg=FALSE)
saveCpiData(CPI)
pred_density_moose_CPI(CPI, digits=3)


## Non ZI models

mZIP <- zeroinfl2(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='poisson', link='logit')
mZINB <- zeroinfl2(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='negbin', link='logit')
mP <- zeroinfl2(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='P', link='logit')
mP2 <- glm(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet,
    data = MooseData[MooseData$srv,], family=poisson)
mNB <- zeroinfl2(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet | 1,
    data = MooseData[MooseData$srv,], dist='NB', link='logit')
mNB2 <- MASS::glm.nb(MOOSE_TOTA ~ TKStrat_01 + NALC_AllNeedleWet,
    data = MooseData[MooseData$srv,])

coef(mZIP)
vcov(mZIP)
coef(mP)
vcov(mP)

coef(mZINB)
vcov(mZINB)
coef(mNB)
vcov(mNB)

round(cbind(coef(mP)[1:3], coef(mP2)), 6)
round(cbind(c(coef(mNB)[1:3], mNB$theta), c(coef(mNB2), mNB2$theta)), 6)
summary(predict(mNB, newdata = MooseData[!MooseData$srv,], type="zero"))

summary(mP)
summary(mZIP)
summary(mNB)
summary(mZINB)

aic <- AIC(mP, mZIP, mNB, mZINB)
aic$dAIC <- aic$AIC - min(aic$AIC)
