#Function to liftOver a file between [hg19] <-> [hg38] 


## Note: hg19(GRCh37)->hg38(GRCh38) OR hg38(GRCh38)->hg19(GRCh37)
## Inputs: hg19 or hg38 


## Command args
args=commandArgs(trailingOnly=T)
path_to_file=args[1] 
liftFrom=args[2] #hg19
liftTo=args[3] #hg38

file_to_lift <- basename(path_to_file)
chain <- paste0(liftFrom, "To", gsub("h","H",liftTo))
path_to_lift <- paste0(dirname(path_to_file),"/", liftTo)


## Load required packages

library(data.table)
library(tidyverse)

library(dplyr)
library(rtracklayer)
library(GenomicRanges)
library(R.utils)



## Download chain.links to opt folder

#hg19ToHg38=https://hgdownload.cse.ucsc.edu/goldenpath/hg19/liftOver/hg19ToHg38.over.chain.gz ; gzip -d opt/hg19ToHg38.over.chain.gz
#hg38ToHg19=https://hgdownload.soe.ucsc.edu/goldenPath/hg38/liftOver/hg38ToHg19.over.chain.gz ; gzip -d opt/hg19ToHg38.over.chain.gz


## Load bim files
bim <- fread(path_to_file) %>% rename(V1="CHR", V2="SNP", V3="POS", V4="BP", V5="ALT", V6="REF")


## Write R function to perform liftOver

liftOver <- function(path_to_file, liftFrom, liftTo, chain, snp_col="SNP", chr_col="CHR", pos_col="POS", ref_col="REF", alt_col="ALT") {
  
  dat.all <- fread(path_to_file) %>% rename(V1="CHR", V2="SNP", V3="POS", V4="BP", V5="ALT", V6="REF")
  dat.input <- dat.all %>% select(SNP=snp_col, CHR=chr_col, POS=pos_col, ALT=alt_col, REF=ref_col)
  
  granges_input <-GRanges(
    seqnames = Rle(paste0("chr", dat.input$CHR)),  # Add 'chr' prefix
    ranges = IRanges(start = dat.input$POS, 
                     end = dat.input$POS + 1),  # Use end = start + 1 for SNPs
    rsid = dat.input$SNP
)
  
  seqlevelsStyle(granges_input) = "UCSC"
  
  chain_file <- rtracklayer::import.chain(paste0("../../opt/", chain,".over.chain"))  # Chain file for liftFrom to liftTo
  ranges.LiftTo <- rtracklayer::liftOver(granges_input, chain_file)
  
  # Extract the converted coordinates
  dat.liftTo <- as.data.frame(unlist(ranges.LiftTo)) %>%
    mutate(CHR=as.numeric(gsub("chr", "", seqnames)), POS=as.numeric(start), SNP=rsid) %>%
    mutate(BP=0)
  
  # Merge lifted data 
  dat.lifted <- dat.input %>% select(SNP, ALT, REF) %>%
    left_join(dat.liftTo, by = "SNP", relationship = "many-to-many") %>%
    select(CHR, SNP, POS, BP, ALT, REF)
  
  # Save lifted files to a sub-folder
  dir.create(path_to_lift, recursive = T)
  dat.lifted %>% fwrite(paste0(path_to_lift, "/", file_to_lift), sep="\t", col.names = F)
  
  return(dat.lifted %>% head())

}

liftOver(path_to_file, liftFrom, liftTo, chain)

##EOF





