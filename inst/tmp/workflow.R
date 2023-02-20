#remotes::install_github("psolymos/moosecounter")
#devtools::document()
#devtools::check()
#devtools::install()
moosecounter::run_app()

x <- read.csv("inst/extdata/MayoMMU_QuerriedData.csv")
switch_response("total")
x <- mc_update_total(x)

## continuous
i <- "SubShrub250_Fire8212DEM815"
mc_plot_univariate(i, x, "ZINB")
.plot_univariate(i, x, dist="ZINB", type = "density")
.plot_univariate(i, x, dist="ZINB", type = "density", interactive = TRUE)

.plot_univariate(i, x, dist="ZINB", type = "map")
.plot_univariate(i, x, dist="ZINB", type = "map", interactive = TRUE)

.plot_univariate(i, x, dist="ZINB", type = "fit")
.plot_univariate(i, x, dist="ZINB", type = "fit", interactive = TRUE)

#
i <- "TKStrat_01"
mc_plot_univariate(i, x, "ZINB")
.plot_univariate(i, x, dist="ZINB", type = "density")


## discrete
i <- "ZZZ"
x$ZZZ <- cut(x$SubShrub250_Fire8212DEM815, 3)
mc_plot_univariate(i, x, "ZINB")

.plot_univariate(i, x, dist="ZINB", type = "density")
.plot_univariate(i, x, dist="ZINB", type = "density", interactive = TRUE)

.plot_univariate(i, x, dist="ZINB", type = "map")
.plot_univariate(i, x, dist="ZINB", type = "map", interactive = TRUE)

.plot_univariate(i, x, dist="ZINB", type = "fit")
.plot_univariate(i, x, dist="ZINB", type = "fit", interactive = TRUE)



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
## results for a subset of rows
pred_density_moose_PI(mc_get_pred(PI, 1:100))

mc_plot_predpi(PI)
mc_plot_pidistr(PI)
mc_plot_pidistr(PI, id=1)

mc_plot_predfit("SubShrub250_Fire8212DEM815", PI)
mc_plot_predfit("SubShrub250_Fire8212DEM815", PI, interactive = TRUE)

# shinyBS shinydashboard
run_app()

shiny::runApp("inst/shiny")

## Composition

# checks comp data (sum of classes should equal total)
mc_check_comp(x)

# plot univariate comp model
mc_plot_comp("Fire8212_DEM815", x)

CML <- list()
CML[['A']] <- mc_fit_comp(x, "Fire8212_DEM815")
CML[['B']] <- mc_fit_comp(x, c("Fire8212_DEM815", "Subalp_Shrub_250buf"))

mc_models_comp(CML)

CPI <- mc_predict_comp(
    total_model_id="Model 3",
    comp_model_id="A",
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE)
# print results: all rows
pred_density_moose_CPI(CPI)
## results for a subset of rows
pred_density_moose_CPI(subset_CPI_data(CPI, 1:100))

head(CPI$cells) # this is what we need
t(apply(CPI$cells, 2, range))

## Gassaway

y1 <- rpois(20, 50)
y2 <- rpois(30, 5)
N1 <- 40
N2 <- 50
mc_gassaway(y1, y2, N1, N2)


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

summary(ML[[2]])

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

## issue

library(moosecounter)
#> This is moosecounter 0.5-1    2022-01-31

## modify options as needed
mc_options(B=20)

## load/read data set
x <- read.csv(system.file("extdata", "UKH2017_MMU_Querried_data_for_Analysis_Final_3.csv", package = "moosecounter"))
switch_response("total")
x <- mc_update_total(x)

# Total models
ML <- list()
ML[["A"]] <- mc_fit_total(x, c("MMU_ID", "AllNeedleWet", "LKStrat_01", "DEM800to1200m"),
                          dist="NB", weighted = FALSE)
ML[["B"]] <- mc_fit_total(x, c("MMU_ID", "AllNeedleWet", "LKStrat_01", "DEM800to1200m"),
                          zi_vars = c("DEM800to1200m", "Fire1982to2012", "NALC_DesShrMix"),
                          dist = "NB", weighted = FALSE)

# Comp models
mc_check_comp(x)
CML <- list()
CML[["A"]] <- mc_fit_comp(x, vars = c("DEM800to1200m", "Fire1982to2012", "NALC_DesShrMix"))

# Error
CPI <- mc_predict_comp(
  total_model_id = c("A", "B"),
  comp_model_id = "A",
  model_list_total = ML,
  model_list_comp = CML,
  x=x,
  do_avg=TRUE)


## new data testing: low abundance

library(moosecounter)
x <- read.csv("~/Dropbox/a8m/projects-2022/yt-000-moosecounter/Cassiar2020_QuerriedSUs_ForFinalAnalysis.csv")
switch_response("total")
# x$UNKNOWN_AG[] <- 0
x <- mc_update_total(x)

mc_options(B=1000)

ML <- list()
ML[["Model 1"]] <- mc_fit_total(x, "SUM_Final_RSPF", "SUM_Final_RSPF", dist="ZINB")

PI <- mc_predict_total(
    model_id="Model 1",
    ml=ML,
    x=x,
    do_boot=TRUE, do_avg=TRUE)

mc_check_comp(x)

CML <- list()
CML[['Comp 1']] <- mc_fit_comp(x)

system.time(CPI1 <- mc_predict_comp(
    total_model_id="Model 1",
    comp_model_id="Comp 1",
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE))
system.time(CPI2 <- mc_predict_comp(
    total_model_id="Model 1",
    comp_model_id="Comp 1",
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE,
    PI=PI))
system.time(CPI3 <- mc_predict_comp(
    total_model_id="Model 1",
    comp_model_id="Comp 1",
    model_list_total=ML,
    model_list_comp=CML,
    x=x,
    do_avg=FALSE,
    PI=PI,
    fix_mean=TRUE))



summary(colSums(PI$boot_full))
summary(sum(rowMeans(PI$boot_full)))
pred_density_moose_PI(PI)
CPI1$total[1,,drop=FALSE]
CPI2$total[1,,drop=FALSE]
CPI3$total[1,,drop=FALSE]
summary(colSums(CPI1$boot_full$Total.pred))
summary(sum(rowMeans(CPI1$boot_full$Total.pred)))
summary(colSums(CPI2$boot_full$Total.pred))
summary(sum(rowMeans(CPI2$boot_full$Total.pred)))
summary(colSums(CPI3$boot_full$Total.pred))
summary(sum(rowMeans(CPI3$boot_full$Total.pred)))


keep <- x$UNKNOWN_AG == 0
# keep <- 1:nrow(x)
table(keep)

pred_density_moose_PI(PI)

summary(colSums(PI$boot_full[keep,]))
summary(sum(rowMeans(PI$boot_full[keep,])))
summary(colSums(CPI2$boot_full$Total.pred))
summary(sum(rowMeans(CPI2$boot_full$Total.pred)))

table(PI$boot_full[keep,1], CPI2$boot_full$Total.pred[,1])
