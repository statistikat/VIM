# Opt-in simulation; real neural runs are deliberately not part of package checks.
# Rscript inst/benchmarks/transformer-mi.R output.rds [replications]
library(VIM)
args<-commandArgs(trailingOnly=TRUE)
output<-if(length(args)) args[1] else tempfile(fileext=".rds")
reps<-if(length(args)>1) as.integer(args[2]) else 100L
methods<-list(
  transformer_pmm=function(d,s) transformer_mi(d,"y",m=5,seed=s,epochs=20,verbose=0),
  ranger_pmm=function(d,s) vimpute(d,method="ranger",m=5,seed=s,boot=TRUE,uncert="pmm",verbose=FALSE)
)
report<-VIM:::.transformer_mi_simulation(methods,replications=reps,output=output)
print(report$summary)
