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


Rscript --vanilla ./sim-script-scanol.R --notify --model P    # done
Rscript --vanilla ./sim-script-scanol.R --notify --model NB   # done
Rscript --vanilla ./sim-script-scanol.R --notify --N 100 --model HNB --seed 1 # -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 100 --model HNB --seed 0 # -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 100 --model ZIP --seed 2 # 11 -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 100 --model ZIP --seed 4 # 14 -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 100 --model HP  --seed 1 # -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 50  --model HP  --seed 10 # -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 50  --model HP  --seed 11 # -- done

Rscript --vanilla ./sim-script-scanol.R --notify --N 100 --model ZINB --seed 0 # -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 50 --model ZINB --seed 14 # 6 -- done
Rscript --vanilla ./sim-script-scanol.R --notify --N 50 --model ZINB --seed 10 # 4 -- done

Rscript --vanilla ./sim-script-scanol.R --notify --N 50 --model ZINB --seed 15 # 7 (14)
Rscript --vanilla ./sim-script-scanol.R --notify --N 25 --model ZINB --seed 30 # 5 (1)
Rscript --vanilla ./sim-script-scanol.R --notify --N 25 --model ZINB --seed 31 # 8 (1)
Rscript --vanilla ./sim-script-scanol.R --notify --N 25 --model ZINB --seed 32 # 9 (1)
Rscript --vanilla ./sim-script-scanol.R --notify --N 25 --model ZINB --seed 33 # 10 (1)

# this is the current
ls -al /root/moosecounter/_tmp/simuls
```

Once finished, copy results back (keep timestamps too):

```bash
# this is the current
rsync -rt \
    root@$IP:/root/moosecounter/_tmp/simuls \
    /Users/Peter/git/github.com/psolymos/moosecounter/_tmp
```


## Use estimates and make reports

```
quarto::quarto_render("inst/simulations/simulation-approach.qmd", output_format = "all")
quarto::quarto_render("inst/simulations/simulation-results.qmd", output_format = "all")
```