# testing install

[This](https://github.com/MTFA/CohortEx/wiki/Run-rJava-with-RStudio-under-OSX-10.10,-10.11-(El-Capitan)-or-10.12-(Sierra)) is needed for Mac.

Piece of advice: use Java 8, don't use Java 9!

Another piece of advice: make sure Java reconfiguration sets the right home,
as described [here](http://www.owsiak.org/r-java-rjava-and-macos-adventures/)
and [here](https://medium.com/@anmol.more/rjava-installation-fix-for-mac-os-25c5caa3f8de).

```R
install.packages(c("Deducer", "pscl", 
    "MuMIn", "partykit", "MASS", "VGAM", "pbapply"))
install.packages(c("JGR","Deducer","DeducerExtras"))
```

Reconfigure Java after updating R to new version:

```R
R CMD javareconf
```

Then you can build and install the package:

```
cd ~/repos

R CMD build DeducerPlugInMoose
R CMD INSTALL DeducerPlugInMoose_0.2-9.tar.gz
```

Launch Deducer via the JGR GUI:

```
# terminal
Rscript -e 'library(Deducer);JGR()'

# or from R console
library(Deducer);JGR()

# in Deducer: load the package to add the Moose menu item
library(DeducerPlugInMoose)
```

