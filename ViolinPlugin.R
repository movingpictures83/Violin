library(microbiome)
library(ggplot2)
#library(phyloseq)
library(ape)
library(psadd)

read_csv2phyloseq_notax <- function(otu.file = NULL, metadata.file = NULL,
    sep = ",")
{
    s.meta <- read.csv(metadata.file, row.names = 1, check.names = FALSE,
        sep = sep)
    s.sampledata <- sample_data(s.meta)
    s.otu <- read.csv(otu.file, row.names = 1, check.names = FALSE,
        sep = sep)
    if (any(rownames(s.otu) %in% rownames(s.meta))) {
        s.otu <- t(s.otu)
    }
    s.otu.table <- otu_table(s.otu, taxa_are_rows = TRUE)
    pseq <- merge_phyloseq(s.otu.table, s.sampledata)
    return(pseq)
}


input <- function(inputfile) {
  print("A")
  parameters <<- read.table(inputfile, as.is=T);
  print("B")
  rownames(parameters) <<- parameters[,1]; 
   # Need to get the three files
   otu.path <<- parameters["otufile", 2]
   #tree.path <<- parameters["tree", 2]
   map.path <<- parameters["mapping", 2]
   diffcol <<- parameters["column", 2]
   #HMP <<- import_qiime(otu.path, map.path, tree.path, parseFunction = parse_taxonomy_qiime)
}
run <- function() {
   #samples.to.keep <<- sample_sums(HMP) >= 1000
   #HMP <<- prune_samples(samples.to.keep, HMP)
   #HMP <<- filter_taxa(HMP, function(x) sum(x >3) > (0.01*length(x)), TRUE)
   print("C")
   print(otu.path)
   print(map.path)
   p0 <<- read_csv2phyloseq_notax(otu.file=otu.path, metadata.file=map.path)#taxonomy.file=tree.path, metadata.file=map.path)
   print("D")
}
output <- function(outputfile) {
  pdf(paste(outputfile,"pdf",sep="."))#,  width = 10*300,        # 5 x 300 pixels
  #height = 10*300); #,)
  print("Generating plot...")
  y <- plot_violin(p0, x=diffcol)
  #print(str(y))
  print("Generating CSV...")
  #print(str(y$data))
  write.csv(y$data, paste(outputfile,"csv",sep="."))
  print(y)#plot_violin(HMP, x="Description", fill=diffcol))
  dev.off()
}
#input("plugins/Bar/example/parameters.txt")
#run()
#output("plugins/Bar/example/yes.pdf")

