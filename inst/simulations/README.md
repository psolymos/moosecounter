# Simulations to validate the abundance estimation approach

# Run scripts on DO

Setup DO server (ubuntu 24.04 LTE, 8 vCPU 16G RAM):

```bash
export IP=68.183.203.109
ssh root@$IP

curl -sSL https://repos.insights.digitalocean.com/install.sh | sudo bash

# https://github.com/eddelbuettel/r-ci/blob/master/docs/run.sh
curl -OLs https://eddelbuettel.github.io/r-ci/run.sh && chmod 0755 run.sh
./run.sh bootstrap

R -q -e "install.packages(c('intrval', 'rconfig', 'devtools', 'pscl', 'partykit', 'pbapply', 'VGAM', 'ggplot2', 'dplyr', 'actuar'))"

exit
```

On local machine, push files up to server:

```bash
rsync -azP \
    --exclude '.git' \
    /Users/Peter/git/github.com/psolymos/moosecounter \
    root@$IP:/root
```

Run R script:

```bash
ssh root@$IP

# tmux new -s mysession
# tmux kill-ses -t mysession
# tmux ls
# tmux a -t mysession
# Ctrl+b then d - detach
# Ctrl+b then x - kill


cd /root/moosecounter/inst/simulations


Rscript --vanilla ./sim-script-scanol.R --model P
Rscript --vanilla ./sim-script-scanol.R --model NB
Rscript --vanilla ./sim-script-scanol.R --model HP
Rscript --vanilla ./sim-script-scanol.R --model HNB
Rscript --vanilla ./sim-script-scanol.R --model ZIP
Rscript --vanilla ./sim-script-scanol.R --model ZINB


# this is the current
ls -al /root/moosecounter/_tmp/simuls
```

Push notifications:

- install app, see: <https://docs.ntfy.sh/>
- set up a topic, e.g. `a8m_cht_alerts`
- send alert `curl -d "Run successful" ntfy.sh/a8m_cht_alerts`

In R:

```R
topic <- "a8m_cht_alerts"
msg <- paste("TEST Finished", YEAR, "@", .POSIXct(Sys.time(), "America/Edmonton"))
system2("curl", c("-d", sprintf("\"%s\"", msg), sprintf("ntfy.sh/%s", topic)))
```

Once finished, copy results back (keep timestamps too):

```bash
# this is the current
rsync -rt \
    root@$IP:/root/bsims-tests/sqpad-paper/analysis/_tmp/est_conv_mc \
    /Users/Peter/git/github.com/psolymos/bsims-tests/sqpad-paper/analysis/_tmp

rsync -rt \
    root@$IP:/root/analysis/_tmp/paired_mc \
    /Users/Peter/git/github.com/psolymos/bsims-tests/sqpad-paper/analysis/_tmp

```


## Use estimates and make reports
```
quarto::quarto_render("inst/simulations/simulation-approach.qmd", output_format = "all")
quarto::quarto_render("inst/simulations/simulation-results.qmd", output_format = "all")
```