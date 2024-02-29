# Intro
`plot.R` script downloads **GENOME_REPORTS** for NCBI and plots two graphs:

* `genomeVSgenes` - illustrates the dependency of Genome Size on the number of genes across 3 domens (namely Eukaryotes, Viruses and Prokaryotes). The goal is to investigate linear dependency charateristic of Prokaryotic and Viral genomes in comparison to non-linear dependency in Eukaryotes

* 'genomes-sequenced' - depicts the cumulative sum of genomes sequenced over the years

# How to run
The data will be downloaded into `\data`, and the outputs will be found at `\plots`
Run with `source("plot.R")`
