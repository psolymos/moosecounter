## global options

.optionsCheckFunction <- function(state){
	#make sure at least two variables are selected
	if (state$method != "") {
		if (is.na(state$method))
	        return("define method")
		if (!(state$method %in% c("Nelder-Mead",
		    "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")))
	        return("provide a valid optim method")
	}
	if (state$response != "") {
		if (is.na(state$response))
	        return("define reponse")
		if (!(tolower(state$response) %in% c("total", "cows")))
	        return("reponse must be 'Total' or 'Cows'")
	}
	if (state$maxcell != "") {
		if(is.na(as.integer(state$maxcell)))
			return("MAXCELL value must be a positive integer")
		if(as.integer(state$maxcell)<0)
			return("MAXCELL value must be a positive integer")
	}
    if (state$b != "") {
		if(is.na(as.integer(state$b)))
			return("Number of bootstrap iterations must be an integer")
		if(as.integer(state$b)<100)
			return("Number of bootstrap iterations must be at least 100")
	}
	if (state$alpha != "") {
		if(is.na(as.numeric(state$alpha)))
			return("Alpha level must be a number between 0 and 1")
		if(as.numeric(state$alpha)>=1)
			return("Alpha level must be a number between 0 and 1")
		if(as.numeric(state$alpha)<=0)
			return("Alpha level must be a number between 0 and 1")
	}
	if (state$wscale != "") {
		if(is.na(as.numeric(state$wscale)))
			return("Weighting scale must be a non-negative number")
		if(as.numeric(state$wscale)<0)
			return("Weighting scale must be a non-negative number")
	}
	if (state$sightability != "") {
		if(is.na(as.numeric(state$sightability)))
			return("Alpha level must be a number between 0 and 1")
		if(as.numeric(state$sightability)>1)
			return("Alpha level must be a number between 0 and 1")
		if(as.numeric(state$sightability)<0)
			return("Alpha level must be a number between 0 and 1")
	}
	return("")
}
.optionsRunFunction <- function(state){

	opts <- getOption("moose_options")
	cmd0 <- if (state$b != "")
		paste0("moose_options(B=", state$b, ")\n") else ""
	cmd1 <- if (state$wscale != "")
		paste0("moose_options(wscale=", state$wscale, ")\n") else ""
	cmd2 <- if (state$alpha != "")
		paste0("moose_options(alpha=", state$alpha, ")\n") else ""
	cmd3 <- if (state$srv != "")
		paste0("moose_options(srv=\"", state$srv, "\")\n") else ""
    cmd4 <- if (state$maxcell != "") {
		paste0("moose_options(MAXCELL=", state$maxcell, ")\n")
    } else {
    	paste0("moose_options(MAXCELL=NULL)\n")
    }
	cmd5 <- if (state$sightability != "")
		paste0("moose_options(sightability=", state$sightability, ")\n") else ""
	cmd6 <- if (state$response != "")
		paste0("switch_response('", state$response, "')\n") else ""
	cmd7 <- if (state$method != "")
		paste0("moose_options(method=\"", state$method, "\")\n") else ""
	cmd8 <- if (state$area_srv != "")
		paste0("moose_options(area_srv=\"", state$area_srv, "\")\n") else ""

	cmd <- paste0(cmd0, cmd1, cmd2, cmd3, cmd4, cmd5, cmd6, cmd7, cmd8)
	if (cmd != "")
		execute(cmd) else invisible(NULL)
}
makeOptionsDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,700L)
	dialog$setTitle("Changing options")

    # order: top, right, bottom, left

	#text area for bootstrap
	textArea <- new(TextAreaWidget, "Number of bootstrap runs (default = 500)")
	textArea$setTitle("b")
	addComponent(dialog, textArea,0,700,80, 10)

	#text area for alpha
	textArea <- new(TextAreaWidget, "Alpha level for prediction interval (default = 0.1)")
	textArea$setTitle("alpha")
	addComponent(dialog, textArea,80,700,160, 10)

	#text area for srv
	textArea <- new(TextAreaWidget, "Surveyed units (e.g. 'Surveyed == 1')")
	textArea$setTitle("srv")
	addComponent(dialog, textArea,160,700,240, 10)

    #text area for maxcell
	textArea <- new(TextAreaWidget, "MAXCELL (maximum total abundance in cell)")
	textArea$setTitle("maxcell")
	addComponent(dialog, textArea,240,700,320, 10)

    #text area for sightability
	textArea <- new(TextAreaWidget, "sightability (number between 0 and 1; default is 1)")
	textArea$setTitle("sightability")
	addComponent(dialog, textArea,200,700,250, 10)

    #text area for response
	textArea <- new(TextAreaWidget, "response ('Total' or 'Cows')")
	textArea$setTitle("response")
	addComponent(dialog, textArea,320,700,400, 10)

	#text area for method
	textArea <- new(TextAreaWidget, "optimization method")
	textArea$setTitle("method")
	addComponent(dialog, textArea,400,700,480, 10)

	#text area for area_srv
	textArea <- new(TextAreaWidget, "Survey area (e.g. 'In1Out0 == 1')")
	textArea$setTitle("area_srv")
	addComponent(dialog, textArea,480,700,560, 10)

	#text area for wscale
	textArea <- new(TextAreaWidget, "Weighting scale (default = 1)")
	textArea$setTitle("wscale")
	addComponent(dialog, textArea,560,700,640, 10)

    #Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,800,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Number of bootstrap runs is the number of times simulations are run\n",
			"for establishing prediction intervals.",
			"\n\nAlpha level sets the Type I error rate for prediction intervals,\n",
			"and also for the multivariate exploration.",
			"\n\nSurveyed units can be described as a logical statement referring to\n",
			"a column name in the data set and a condition.",
            "\n\nMAXCELL is twice the maximum observed cell-wise abundance,\n",
            "but can be set to an arbitrary value that is not less than the\n",
            "observed maximum.",
            "\n\nsightability is detection probability between 0 and 1\n",
            "that is used to correct the total Moose results.",
			"\n\nResponse 'Total' vs. 'Cows' sets to response variable.",
			"\n\nMethod defines the optimization algorithm (default is 'BFGS', can be e.g. 'Nelder-Mead').",
			"\n\nWeighting scale is a non-negative number for dealing with influential observations, default is 1, 0 produces equal weights."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.optionsCheckFunction))
	dialog$setRunFunction(toJava(.optionsRunFunction))
	return(dialog)
}

## Tree

.treeCheckFunction <- function(state){
	#make sure at least two variables are selected
	if(length(state$variables)<2)
		return("Please select at least two variables")
	return("")
}
.treeRunFunction <- function(state){
	opts <- getOption("moose_options")
	Terms <- state$variables
	Terms <- Terms[!(Terms %in% c(opts$Ntot, opts$composition))]
	Form <- paste(opts$Ntot, "~", paste(Terms, collapse=" + "))
	cmd <- paste0(
			"saveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv), ")",
			"\nmoose.tree <- ctree(", Form, ", data = ",
#			state$data, "[", paste0(state$data, "$", opts$srv), ",],",
			"MooseData[MooseData$srv,],",
			" control = ctree_control(mincriterion = ", 1 - opts$alpha, "))")
	## print and plot
	cmd <- paste0(cmd,"\nprint(moose.tree)\nplot(moose.tree)")
	#execute command as if typed into console
	execute(cmd)
}
makeTreeDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Multivariate exploration")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)

	#add a list for the variables
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("variables")
	addComponent(dialog, variableList,100,900,700, 420)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Select the imported Moose data table from the top left drop-down.\n",
			"Move variables from the left panel to the right panel using the arrows.\n",
			"The regression tree will show a hierarchy of splits. The higher\n",
			"a variable is in the hierarchy the greater the effect on total\n",
			"Moose abundance. The values at each split separate low (left node)\n",
			"and high (right node) abundance strata."
			))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.treeCheckFunction))
	dialog$setRunFunction(toJava(.treeRunFunction))
	return(dialog)
}

## subset the data

.subsetCheckFunction <- function(state){
# no variable means reset
#	if(length(state$variable)<1)
#		return("Please select one variable")
	if(length(state$variable) == 0 && state$criter != "")
		return("Please select a variable for the logical expression")
	if(length(state$variable) == 0 && state$criter == "")
		return("")
	if(length(state$variable)>1)
		return("Please select one variable")
  if (state$variable != "" && state$criter == "")
		return("Please define a logical expression")
	if (state$variable == "" && state$criter != "")
		return("Please select a variable for the logical expression")
	return("")
}
.subsetRunFunction <- function(state){
	opts <- getOption("moose_options")

	if (state$criter == "") {
		cmd <- paste0(
				"saveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv),
				", force=TRUE)")
	} else {
		Eval <- paste0(paste0(state$data, "$", state$variable), " ", state$criter)
		cmd <- paste0(
				"saveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv),
				",", Eval, ", force=TRUE)")
	}
	#execute command as if typed into console
	execute(cmd)
}
makeSubsetDialog <- function(){

	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Subset the data")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,900,10)

	#add a list for the variables
	variableList<- new(SingleVariableWidget,variableSelector)
	variableList$setTitle("variable")
	addComponent(dialog, variableList,100,900,200, 420)

	#text area for model name
	textArea <- new(TextAreaWidget, "Logical expression")
	textArea$setTitle("criter")
	addComponent(dialog, textArea,550,900,700, 420)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Select the imported Moose data table from the top left drop-down.\n",
			"Move a variable from the left panel to the right using the arrows.\n",
			"Define a condition using logical operators and values of the selected\n",
			"variable. The subset is going to be in effect (i.e. subsequent analyses\n",
			"will be based on the subset) until it is reset.\n",
			"To reset the subsetting: click 'Reset' (this removes the values),\n",
			"then click 'Run'."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.subsetCheckFunction))
	dialog$setRunFunction(toJava(.subsetRunFunction))
	return(dialog)
}

## density plots

.densityCheckFunction <- function(state){
	#make sure at least two variables are selected
	if(length(state$variable)<1)
		return("Please select one variable")
	if(length(state$variable)>1)
		return("Please select one variable")
	return("")
}
.densityRunFunction <- function(state){
    dist <- if (state$dist == "ZINB")
        "negbin" else "poisson"
	opts <- getOption("moose_options")
	cmd <- paste0(
		"saveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv), ")",
		"\nplotUnivariateExpl('", state$variable, "', dist='", dist, "')")
	#execute command as if typed into console
	execute(cmd)
}
makeDensityDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Univariate exploration")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)

	#add a list for the variables
	variableList<- new(SingleVariableWidget,variableSelector)
	variableList$setTitle("variable")
	addComponent(dialog, variableList,100,900,200, 420)

	radio <- new(ButtonGroupWidget, c("ZINB", "ZIP"))
	radio$setTitle("dist")
	addComponent(dialog, radio, 400,850,600,500)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste(
			"Select the imported Moose data table from the top left drop-down.\n",
			"Move a variable from the left panel to the right using the arrows.\n",
			"The result is a (1) density of the selected variable showing\n",
			"density for all available cells and for the surveyed ones, (2) a heatmap,\n",
			"and a (3) log-linear relationship with total Moose abundance.\n",
			"The radio button can be used to select ZINB or ZIP model."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.densityCheckFunction))
	dialog$setRunFunction(toJava(.densityRunFunction))
	#dialog$run()
	return(dialog)
}

## define ZINB/ZIP model

.totalCheckFunction <- function(state){
	#make sure at least two variables are selected
	if(length(state$variables)<1)
		return("Please select at least one dependent variable")
	if(state$modelid == "")
		return("Please type in a model ID")
	return("")
}
.totalRunFunction <- function(state){
    #dist <- if (state$dist == "ZINB")
    #    "negbin" else "poisson"
    dist <- switch(state$dist,
        "P"="P",
        "NB"="NB",
        "ZIP"="poisson",
        "ZINB"="negbin")
	opts <- getOption("moose_options")
	ModID <- make.names(state$modelid)
	Terms <- state$variables
	Terms <- Terms[!(Terms %in% c(opts$Ntot, opts$composition))]
	CNT <- paste(Terms, collapse=" + ")
	if (length(state$ZIvariables) < 1) {
	    ZI <- "1"
	} else {
    	TermsZI <- state$ZIvariables
    	TermsZI <- TermsZI[!(TermsZI %in% c(opts$Ntot, opts$composition))]
    	ZI <- paste(TermsZI, collapse=" + ")
	}
	Form <- paste(opts$Ntot, "~", CNT, "|", ZI)
	w <- if (state$weighted == "Weighted")
	    "wzi(" else "("
	cmd <- paste0("checkModelList()",
			"\nsaveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv), ")",
			"\nModelList[['", ModID, "']] <- ", w, "zeroinfl2(",
			Form, ", data = ",
			#state$data, "[", paste0(state$data, "$", opts$srv), ",],",
			"MooseData[MooseData$srv,],",
			" dist='", dist, "', link='logit'))")
	## print and plot
	cmd <- paste0(cmd, "\ncat('Summary for Model ID: ", ModID,
			"\n')\nsummary(ModelList[['", ModID,
			"']])\nplotResiduals('", ModID,"')\nprint(updateModelTab())")
	#execute command as if typed into console
	execute(cmd)
}
makeTotalDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,600L)
	dialog$setTitle("Model fitting for total moose")

	#add variable selector: count
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,300,10)

	#add a list for the variables: count
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("variables")
	addComponent(dialog, variableList,100,900,300, 420)

	#add variable selector: zeros
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,310,400,600,10)

	#add a list for the variables: zeros
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("ZIvariables")
	addComponent(dialog, variableList,400,900,600, 420)

    txt1 <- new(JLabel, "Count part:")
	addComponent(dialog, txt1, 50,900,100,600)
    txt2 <- new(JLabel, "Zero part:")
	addComponent(dialog, txt2, 350,900,400,600)

	#text area for model name
	textArea <- new(TextAreaWidget, "Model ID")
	textArea$setTitle("modelid")
	addComponent(dialog, textArea,650,900,750, 500)

	radio <- new(ButtonGroupWidget, c("P", "NB", "ZIP", "ZINB"))
	radio$setTitle("dist")
	addComponent(dialog, radio, 650,400,850,50)

	radio <- new(ButtonGroupWidget, c("Non weighted", "Weighted"))
	radio$setTitle("weighted")
	addComponent(dialog, radio, 870,400,970,50)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,800,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Select the imported Moose data table from the top left drop-down.\n",
			"Move variables from the left panel to the right panel using the arrows:\n",
			"the top selector contains variables for the count part of the model,\n",
			"the bottom selector contains variables for the zero part of the model.",
			"The Zero-inflated Negative Binomial model will include these\n",
			"variables as predictors with total Moose abundance as the response.\n",
			"The user must provide a model ID so that multiple models can be compared.\n",
			"Model summaries and AIC table are printed, residuals are plotted.\n",
			"ZIP and ZINB are different count models, with or without weights."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.totalCheckFunction))
	dialog$setRunFunction(toJava(.totalRunFunction))
	return(dialog)
}

## plot resuduals with labels

.residCheckFunction <- function(state){
	#make sure at least two variables are selected
	#print(state)
	if(length(state$model)<1)
		return("Please select at least one model")
	if(length(state$model)>1)
		return("Please select only one model")
	return("")
}
.residRunFunction <- function(state){

	cmd <- paste0("plotResiduals('", state$model, "')")
	#execute command as if typed into console
	execute(cmd)
}
makeResidDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Residual plots")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)

	#add a list for the variables
	variableList<- new(SingleVariableWidget,variableSelector)
	variableList$setTitle("model")
	addComponent(dialog, variableList,100,900,200, 420)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Select the 'ModelTab' data table from the top left drop-down.\n",
			"Select the model ID for which residuals are to be plotted\n",
			"and move the model ID to the right using the arrow.\n"
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.residCheckFunction))
	dialog$setRunFunction(toJava(.residRunFunction))
	#dialog$run()
	return(dialog)
}

## prediction intervals

.piCheckFunction <- function(state){
	#make sure at least two variables are selected
	#print(state)
#    if (state$average == "Model averaging")
#        return("")
	if(length(state$model)<1)
		return("Please select at least one model")
	#if(length(state$model)>1)
	#	return("Please select only one model")
	return("")
}
.piRunFunction <- function(state){

    Avg <- if (state$average == "Model averaging")
        "TRUE" else "FALSE"
    ss <- paste0("c('", paste0(state$model, collapse="','"), "')")
	cmd <- paste0("PI <- MooseSim.PI(", ss, ", do_avg=",
        Avg, ")",
	"\nsavePiData(PI)\nplot_predPI(PI)\npred_density_moose_PI(PI)")
	#execute command as if typed into console
	execute(cmd)
}
makePiDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Prediction intervals for total abundance")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)

	#add a list for the variables
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("model")
	addComponent(dialog, variableList,100,900,450, 420)

	radio <- new(ButtonGroupWidget, c("Selected model", "Model averaging"))
	radio$setTitle("average")
	addComponent(dialog, radio, 500,850,700,500)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Select the 'ModelTab' data table from the top left drop-down.\n",
			"Select the model ID for which prediction intervals are to be estimated\n",
			"and move the model ID to the right using the arrow.\n",
			"Be patient, bootstrap runs might take a while.\n",
			"The results include estimated total Moose abundances, model residuals,\n",
			"and prediction accuracy (wide prediction interval is in darker color,\n",
			"cell IDs are plotted as well for convenience).\n",
			"Look for 'PiData' in the Data Viewer with the cell level results.\n",
			"Best model pick the best model from the selected ones,\n",
			"model averaging used AIC weights to average the models."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.piCheckFunction))
	dialog$setRunFunction(toJava(.piRunFunction))
	#dialog$run()
	return(dialog)
}

## plot PI distribution with quantiles and central tendency

.pidistCheckFunction <- function(state){
	return("")
}
.pidistRunFunction <- function(state){

	What <- if (state$radio == "Full data set")
	    "PI" else "PIsubset"
	br <- if (state$breaks == "")
	    "'Sturges'" else state$breaks
	cmd <- paste0("summary(PlotPiDistr(", What, ", breaks=", br, "))")
	execute(cmd)
}
makeDistDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(400L,300L)
	dialog$setTitle("Plot prediction distribution")

	radio <- new(ButtonGroupWidget, c("Full data set", "Data subset only"))
	radio$setTitle("radio")
	addComponent(dialog, radio, 1, 500, 200, 1)

	#text area for model name
	textArea <- new(TextAreaWidget, "Histogram Breaks")
	textArea$setTitle("breaks")
	addComponent(dialog, textArea,300,800,500, 200)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Plots prediction distribution for an existing PI object,\n",
			"either the full data set or for the subset.\n",
			"The plot shows the probability density of the prediction\n",
			"distribution as histogram, density estimate, with various\n",
			"measures of central tendency and prediction interval.\n",
			"Type in the number of breaks desired."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.pidistCheckFunction))
	dialog$setRunFunction(toJava(.pidistRunFunction))
	#dialog$run()
	return(dialog)
}



## subset PI table

.pisubsetCheckFunction <- function(state){
	#make sure at least two variables are selected
	return("")
}
.pisubsetRunFunction <- function(state){

	cmd <- if (state$ss != "")
		paste0("PIsubset <- subsetPiData(PI, PI$data$", state$ss, ")",
			"\nsavePiDataSubset(PIsubset)\n",
            "\nplot_predPI(PIsubset)\n",
            "pred_density_moose_PI(PIsubset)") else ""
    if (cmd != "")
		execute(cmd) else invisible(NULL)
}
makePisubsetDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,500L)
	dialog$setTitle("Subset total Moose prediction table")

	#text area for bootstrap
	textArea <- new(TextAreaWidget, "Subset (e.g. 'MMU == 1')")
	textArea$setTitle("ss")
	addComponent(dialog, textArea,100,700,250, 10)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,300,800,400,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"This dialog subsets the existing prediction interval results\n",
			"based on some condition defined by a logical statement.\n",
			"Look for 'PiDataSubset' in the Data Viewer with the cell level results\n",
			"within the subset."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.pisubsetCheckFunction))
	dialog$setRunFunction(toJava(.pisubsetRunFunction))
	return(dialog)
}

## fit composition model

.cfitCheckFunction <- function(state){
	#make sure at least two variables are selected
	#if(length(state$variables)<1)
	#	return("Please select at least one variable")
	if(state$modelid == "")
		return("Please type in a model ID")
	return("")
}
.cfitRunFunction <- function(state){
	opts <- getOption("moose_options")
	ModID <- make.names(state$modelid)
	Terms <- state$variables
	if (length(Terms) < 1) {
		Form <- "~ 1"
	} else {
		Terms <- Terms[!(Terms %in% c(opts$Ntot, opts$composition))]
		Form <- paste("~", paste(Terms, collapse=" + "))
	}
	cmd <- paste0(
			"checkCompModelList()\n",
			"saveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv), ")",
			"\ncheckCompData()\n",
			"CompModelList[['", ModID, "']] <- fitCompModel(", Form, ")\n",
			"cat('Summary for Composition Model ID: ", ModID,
			"\n')\nsummary(CompModelList[['", ModID, "']])\n",
			"print(updateCompModelTab())")
	#execute command as if typed into console
	execute(cmd)
}
makeCfitDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Fit composition model")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,900,10)

	#add a list for the variables
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("variables")
	addComponent(dialog, variableList,100,900,500, 420)

	#text area for model name
	textArea <- new(TextAreaWidget, "Model ID")
	textArea$setTitle("modelid")
	addComponent(dialog, textArea,550,900,700, 420)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Select the imported Moose data table from the top left drop-down.\n",
			"Move variables from the left panel to the right panel using the arrows.\n",
			"If no variables selected a constant (intercept only) model is assumed.\n",
			"The Multinomial model will include these variables as predictors\n",
			"with composition data as the response.\n",
			"The user must provide a model ID so that multiple models can be compared.\n",
			"Model summaries and AIC table are printed."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.cfitCheckFunction))
	dialog$setRunFunction(toJava(.cfitRunFunction))
	return(dialog)
}



## Comp PI
.cpiCheckFunction <- function(state){
	if(length(state$totalid)<1)
		return("Please select at least one Moose abundance model ID")
	if(length(state$compid)<1)
		return("Please select one composition model ID")
	return("")
}
.cpiRunFunction <- function(state){

    Avg <- if (state$average == "Model averaging")
        "TRUE" else "FALSE"
    ss <- paste0("c('", paste0(state$totalid, collapse="','"), "')")
	cmd <- paste0("CPI <- MooseCompSimMMU.PI1(",
		ss, ", '", state$compid, "', do_avg=", Avg, ")",
        "\nsaveCpiData(CPI)",
        "\npred_density_moose_CPI(CPI, digits=3)")
	execute(cmd)
}
makeCpiDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,600L)
	dialog$setTitle("Prediction intervals for composition")

	#add model selector: total
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("totaldata")
	addComponent(dialog,variableSelector,10,400,300,10)

	#add a list for the models: total
	variableList<- new(VariableListWidget,variableSelector)
	variableList$setTitle("totalid")
	addComponent(dialog, variableList,100,900,300, 420)

	#add model selector: comp
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("compdata")
	addComponent(dialog,variableSelector,310,400,600,10)

	#add a single model: comp
	variableList<- new(SingleVariableWidget,variableSelector)
	variableList$setTitle("compid")
	addComponent(dialog, variableList,400,900,500, 420)

    txt1 <- new(JLabel, "Total abund. model ID")
	addComponent(dialog, txt1, 50,900,100,600)
    txt2 <- new(JLabel, "Composition model ID")
	addComponent(dialog, txt2, 350,900,400,600)

	radio <- new(ButtonGroupWidget, c("Selected model", "Model averaging"))
	radio$setTitle("average")
	addComponent(dialog, radio, 700,450,900,100)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,800,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"Total Moose and composition model IDs are mandatory.\n",
			"Select one or more total abundance model IDs, and check\n",
			"radio box if model averaging is needed.\n",
			"Pick a single composition model ID."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.cpiCheckFunction))
	dialog$setRunFunction(toJava(.cpiRunFunction))
	return(dialog)
}

## univariate composition plot

.plotcompCheckFunction <- function(state){
	#make sure at least two variables are selected
	if(length(state$variable)<1)
		return("Please select one variable")
	if(length(state$variable)>1)
		return("Please select one variable")
	return("")
}
.plotcompRunFunction <- function(state){
	opts <- getOption("moose_options")
	#Data <- state$data
	#Srv <- paste0(Data, "$", opts$srv)
	cmd <- paste0(
		"saveMooseData(",state$data,", ", paste0(state$data, "$", opts$srv), ")",
		"\nplotComp('", state$variable, "')")
#	Var <- paste0(state$data, "$", state$variable)
#	cmd <- paste0("by(", Var, ", ", Srv,
#			", summary)\nplotUnivariateExpl(", Data, ",", Srv, ",'", state$variable, "')")
	#execute command as if typed into console
	execute(cmd)
}
makePlotcompDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,400L)
	dialog$setTitle("Composition exploration")

	#add variable selector
	variableSelector <- new(VariableSelectorWidget)
	variableSelector$setTitle("data")
	addComponent(dialog,variableSelector,10,400,850,10)

	#add a list for the variables
	variableList<- new(SingleVariableWidget,variableSelector)
	variableList$setTitle("variable")
	addComponent(dialog, variableList,100,900,200, 420)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,750,800,850,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste(
			"Select the imported Moose data table from the top left drop-down.\n",
			"Move a variable from the left panel to the right using the arrows.\n",
			"The result shows how composition varies with the selected predictor."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.plotcompCheckFunction))
	dialog$setRunFunction(toJava(.plotcompRunFunction))
	#dialog$run()
	return(dialog)
}

## subset CPI table

.cpisubsetCheckFunction <- function(state){
    #make sure at least two variables are selected
	return("")
}
.cpisubsetRunFunction <- function(state){

	cmd <- if (state$ss != "")
		paste0("CPIsubset <- subsetCpiData(CPI, CPI$data$", state$ss, ")",
			"\nsaveCpiDataSubset(CPIsubset)\n",
            "\npred_density_moose_CPI(CPIsubset, digits=3)") else ""
    if (cmd != "")
		execute(cmd) else invisible(NULL)
}
makeCpisubsetDialog <- function(){
	#make dialog
	dialog <- new(SimpleRDialog)
	dialog$setSize(500L,500L)
	dialog$setTitle("Subset composition prediction table")

	#text area for bootstrap
	textArea <- new(TextAreaWidget, "Subset (e.g. 'MMU == 1')")
	textArea$setTitle("ss")
	addComponent(dialog, textArea,100,700,250, 10)

	#Add an 'Options' button
	JButton <- J("javax.swing.JButton")
	button <- new(JButton,"Help")
	addComponent(dialog,button,300,800,400,600)
	#Listen for the button to be pressed
	ActionListener <- J("org.rosuda.deducer.widgets.event.RActionListener")
	listener <- new(ActionListener)
	JOptionPane <- J("javax.swing.JOptionPane")
	actionFunction <- function(cmd,ActionEvent){
		JOptionPane$showMessageDialog(dialog,paste0(
			"This dialog subsets the existing prediction interval results\n",
			"based on some condition defined by a logical statement.\n",
			"Look for 'PiDataSubset' in the Data Viewer with the cell level results\n",
			"within the subset."
		))
	}
	listener$setFunction(toJava(actionFunction))
	button$addActionListener(listener)

	dialog$setCheckFunction(toJava(.cpisubsetCheckFunction))
	dialog$setRunFunction(toJava(.cpisubsetRunFunction))
	return(dialog)
}

## zzz

.onUnload <- function(libpath){
    options("moose_options"=NULL,
        "scipen"=0)
    invisible(NULL)
}
.onLoad <- function(libname, pkgname) {

    #if deducer gui is not running, do minimal load
	deducerLoaded <- try(.deducer == .jnull(),silent=TRUE)
	if(inherits(deducerLoaded,"try-error") || deducerLoaded)
		return(NULL)

	#loads example.jar
	.jpackage(pkgname,lib.loc=libname)



	## on load -- menu items

	.registerDialog("Change options", makeOptionsDialog)
	.registerDialog("Subset data", makeSubsetDialog)
	.registerDialog("Univariate exploration", makeDensityDialog)
	.registerDialog("Multivariate exploration", makeTreeDialog)
	.registerDialog("Fit total moose model", makeTotalDialog)
	.registerDialog("Plot residuals", makeResidDialog)
	.registerDialog("Estimate pred. intervals", makePiDialog)
	.registerDialog("Subset pred. results", makePisubsetDialog)
	.registerDialog("Plot pred. distribution", makeDistDialog)
	.registerDialog("Composition exploration", makePlotcompDialog)
	.registerDialog("Fit composition model", makeCfitDialog)
	.registerDialog("Estimate composition intervals", makeCpiDialog)
    .registerDialog("Subset composition results", makeCpisubsetDialog)

	#add menu items
	deducer.addMenu("MooseSurvey")
	deducer.addMenuItem("Change options",,".getDialog('Change options')$run()","MooseSurvey")
	deducer.addMenuItem("Subset data",,".getDialog('Subset data')$run()","MooseSurvey")
	deducer.addMenuItem("Univariate exploration",,".getDialog('Univariate exploration')$run()","MooseSurvey")
	deducer.addMenuItem("Multivariate exploration",,".getDialog('Multivariate exploration')$run()","MooseSurvey")
	deducer.addMenuItem("Fit total moose model",,".getDialog('Fit total moose model')$run()","MooseSurvey")
	deducer.addMenuItem("Plot residuals",,".getDialog('Plot residuals')$run()","MooseSurvey")
	deducer.addMenuItem("Estimate pred. intervals",,".getDialog('Estimate pred. intervals')$run()","MooseSurvey")
	deducer.addMenuItem("Subset pred. results",,".getDialog('Subset pred. results')$run()","MooseSurvey")
	deducer.addMenuItem("Plot pred. distribution",,".getDialog('Plot pred. distribution')$run()","MooseSurvey")
	deducer.addMenuItem("Composition exploration",,".getDialog('Composition exploration')$run()","MooseSurvey")
	deducer.addMenuItem("Fit composition model",,".getDialog('Fit composition model')$run()","MooseSurvey")
	deducer.addMenuItem("Estimate composition intervals",,".getDialog('Estimate composition intervals')$run()","MooseSurvey")
    deducer.addMenuItem("Subset composition results",,".getDialog('Subset composition results')$run()","MooseSurvey")
	if(.windowsGUI){
		winMenuAdd("MooseSurvey")
		winMenuAddItem("MooseSurvey", "Change options", "deducer('Change options')")
		winMenuAddItem("MooseSurvey", "Subset data", "deducer('Subset data')")
		winMenuAddItem("MooseSurvey", "Univariate exploration", "deducer('Univariate exploration')")
		winMenuAddItem("MooseSurvey", "Multivariate exploration", "deducer('Multivariate exploration')")
		winMenuAddItem("MooseSurvey", "Fit total moose model", "deducer('Fit total moose model')")
		winMenuAddItem("MooseSurvey", "Plot residuals", "deducer('Plot residuals')")
		winMenuAddItem("MooseSurvey", "Estimate pred. intervals", "deducer('Estimate pred. intervals')")
		winMenuAddItem("MooseSurvey", "Subset pred. results", "deducer('Subset pred. results')")
		winMenuAddItem("MooseSurvey", "Plot pred. distribution", "deducer('Plot pred. distribution')")
		winMenuAddItem("MooseSurvey", "Composition exploration", "deducer('Composition exploration')")
		winMenuAddItem("MooseSurvey", "Fit composition model", "deducer('Fit composition model')")
		winMenuAddItem("MooseSurvey", "Estimate composition intervals", "deducer('Estimate composition intervals')")
    	winMenuAddItem("MooseSurvey", "Subset composition results", "deducer('Subset composition results')")
	}else if(.jgr){
		jgr.addMenu("MooseSurvey")
		jgr.addMenuItem("MooseSurvey", "Change options", "deducer('Change options')")
		jgr.addMenuItem("MooseSurvey", "Subset data", "deducer('Subset data')")
		jgr.addMenuItem("MooseSurvey", "Univariate exploration", "deducer('Univariate exploration')")
		jgr.addMenuItem("MooseSurvey", "Multivariate exploration", "deducer('Multivariate exploration')")
		jgr.addMenuItem("MooseSurvey", "Fit total moose model", "deducer('Fit total moose model')")
		jgr.addMenuItem("MooseSurvey", "Plot residuals", "deducer('Plot residuals')")
		jgr.addMenuItem("MooseSurvey", "Estimate pred. intervals", "deducer('Estimate pred. intervals')")
		jgr.addMenuItem("MooseSurvey", "Subset pred. results", "deducer('Subset pred. results')")
		jgr.addMenuItem("MooseSurvey", "Plot pred. distribution", "deducer('Plot pred. distribution')")
		jgr.addMenuItem("MooseSurvey", "Composition exploration", "deducer('Composition exploration')")
		jgr.addMenuItem("MooseSurvey", "Fit composition model", "deducer('Fit composition model')")
		jgr.addMenuItem("MooseSurvey", "Estimate composition intervals", "deducer('Estimate composition intervals')")
    	jgr.addMenuItem("MooseSurvey", "Subset composition results", "deducer('Subset composition results')")
	}

	invisible(NULL)
}
.onAttach <- function(libname, pkgname) {
	ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
								fields=c("Version", "Date"))
	packageStartupMessage(paste("This is", pkgname, ver[1], "\t", ver[2]))

	if (is.null(getOption("moose_options")))
		options(
		    "scipen"=999,
		    "moose_options"=list(
		        method = "Nelder-Mead",
    			B=500,
    			MAXCELL=NULL, # this is max possible total abundance in a cell
    			alpha=0.1,
    			wscale=1,
    			MINCELL=10, # this is min number of cells for composition
    			Ntot="MOOSE_TOTA",
    			srv="Sampled==1",#"CENSUS_ID==7",
    			area_srv=NULL,
    			Area="AREA_KM",
    			sightability=1,
    			response="total", # total/cows
    			xy=c("CENTRLON", "CENTRLAT"),
    			composition=c("BULL_SMALL", "BULL_LARGE", "LONE_COW",
    					"COW_1C", "COW_2C", "LONE_CALF", "UNKNOWN_AG")))

	pboptions(type="none")

	invisible(NULL)
}

