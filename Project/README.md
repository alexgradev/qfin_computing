# Eurovision Project

**Authors:** Group 1 (Bachtiary, Gradev, Hartai, Isakova, Pemsel)

**Course:** QFin Computing WS26/27 (WU Vienna)

### How to compile the slides

The slides read their numbers from `03_results/` and `04_figures/`, which are in
the repo, so everyone can compile without rerunning the analysis.

Needs R and a LaTeX installation with beamer (e.g. `tinytex`).

For MacOS:
```bash
cd Project/05_slides
R CMD Sweave eurovision.Rnw
pdflatex eurovision.tex
bibtex eurovision
pdflatex eurovision.tex
pdflatex eurovision.tex
```

For Windows/Linux ask AI.

### Rerun the analysis

Requires R packages `igraph`, `readxl`, `jsonlite` 

To rerun `build_data.R` clone https://github.com/EurovisionAPI/dataset
into `01_data/eurovision-api`.

Run the scripts from the `Project` folder, `build_data.R` first:
```bash
cd Project
Rscript 02_scripts/build_data.R
Rscript 02_scripts/hook.R
Rscript 02_scripts/bloc_favoritism.R
Rscript 02_scripts/network.R
Rscript 02_scripts/social_ties.R
```