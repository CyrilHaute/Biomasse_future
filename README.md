# Biomasse_future

Research compendium to reproduce analyses and figures of the following article:

Cyril Hautecoeur, Ulysse Flandrin, Martin Paquet, Jerry Tjiputra, Ella Clausius,
Graham Edgar, Rick Stuart-Smith, Eva Maire, Laure Velez, Nicolas Loiseau, 
David Mouillot, Camille Mellin (2026), 
**One-fourth of coral reef fishes at risk of extinction under future climate and socioeconomic changes**

Analyses were done with **R** version **4.6.1**.

To clone the repository from [GitHub](https://github.com/), open R and
go to your Terminal or open directly a Terminal and do:

``` ruby
git clone https://github.com/CyrilHaute/Biomasse_future
```

Required dependencies can be found in the `DESCRIPTION` file and can be
installed and load with the following function:

``` ruby
## Install devtools package ----
install.packages("devtools")

## Install required package ----
devtools::install_deps(upgrade = "never")
```

The repository is structured as follow:

- `data/`: the `data` file can be found at this [Zenodo](https://zenodo.org/) link: https://zenodo.org/records/24105253.
It contains all raw and derived data to reproduce the analysis in open access.
- `R/`: contains all functions.
- `analyses/`: contains scripts to load data, run `R/` functions and reproduce figures.
- `results/`: contains all results. 
The results miss the ensemble model files (**migration_ensemble8.rds** and **withoutmigration_ensemble8.rds**) as they are heavy. They can
be easily reproduce with the `analyses/12_future_predictions.R` file.
- `figures/`: contains all figures.