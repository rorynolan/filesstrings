pkgname <- "SSHAARP"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('SSHAARP')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("BLAASD")
### * BLAASD

flush(stderr()); flush(stdout())

### Name: BLAASD
### Title: BLAASD - Build Loci Amino Acid Specific Dataframe
### Aliases: BLAASD

### ** Examples

#BLAASD with one locus as input

#BLAASD with multiple loci as input



cleanEx()
nameEx("PALM")
### * PALM

flush(stderr()); flush(stdout())

### Name: PALM
### Title: Population Allele Locating Mapmaker
### Aliases: PALM

### ** Examples

#example to produce a color frequency heat map where migrant populations are filtered out
PALM("DRB1*26F~28E~30Y",filename = solberg_dataset[85:100,], filterMigrant=TRUE)
#example to produce a greyscale heat map where migrant populations are not filtered out
PALM("DRB1*26F~28E~30Y", filename = solberg_dataset[85:100,], color=FALSE, filterMigrant=FALSE)




cleanEx()
nameEx("checkMotif")
### * checkMotif

flush(stderr()); flush(stdout())

### Name: checkMotif
### Title: Syntactic and semantic validation of HLA amino acid motifs
### Aliases: checkMotif

### ** Examples

#Example where a motif is formatted correctly

#Example where format is incorrect

#Example where an amino acid position does not exist



cleanEx()
nameEx("findMotif")
### * findMotif

flush(stderr()); flush(stdout())

### Name: findMotif
### Title: Returns an alignment data frame of alleles that share a specific
###   amino acid motif
### Aliases: findMotif

### ** Examples


#example with actual motif

#example with non-existent motif

#extracting names of alleles with user-defined motif
findMotif("DRB1*26F~28E~30Y")[,4]



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
