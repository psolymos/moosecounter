.onUnload <- function(libpath){
    options("moose_options"=NULL)
    invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
        fields=c("Version", "Date"))
    packageStartupMessage(paste("This is", pkgname, ver[1], "\t", ver[2]))
    if (is.null(getOption("moose_options")))
        options(
            "moose_options"=list(
                method = "Nelder-Mead",
                B=500,
                MAXCELL=NULL, # this is max possible total abundance in a cell
                alpha=0.1,
                wscale=1,
                MINCELL=10, # this is min number of cells for composition
                Ntot="MOOSE_TOTA",
                srv_name="Sampled",
                srv_value=1,
                area_srv=NULL,
                Area="AREA_KM",
                sightability=1,
                response="total", # total/cows
                xy=c("CENTRLON", "CENTRLAT"),
                composition=c("BULL_SMALL", "BULL_LARGE", "LONE_COW",
                        "COW_1C", "COW_2C", "LONE_CALF", "UNKNOWN_AG")))
    invisible(NULL)
}
