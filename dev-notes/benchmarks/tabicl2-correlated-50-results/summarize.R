source('dev-notes/benchmarks/tabicl2-correlated.R')
output<-'.local/benchmarks/tabicl2-correlated-y-only-typed-epochs20-50reps'
result<-tabicl_correlated_summary(output)
stopifnot(nrow(result$metrics)==150L,all(result$summary$repetitions==50L),
          all(is.finite(result$metrics$rmse)),all(result$metrics$n_masked==32L))
folder<-'dev-notes/benchmarks/tabicl2-correlated-50-results'
for(name in c('metrics','summary','paired'))
 write.csv(result[[name]],file.path(folder,paste0(name,'.csv')),row.names=FALSE)
for(file in c('config.rds','run-info.txt'))
 stopifnot(file.copy(file.path(output,file),file.path(folder,file),overwrite=TRUE))
writeLines(capture.output(sessionInfo()),file.path(folder,'session.txt'))
library(ggplot2)
methods<-result$config$config$methods
labels<-c('Pretrained','Fine-tuned','Fine-tuned\nsequential')
d<-result$metrics
d$variant<-factor(d$method,levels=methods,labels=labels)
p<-ggplot(d,aes(variant,rmse))+geom_line(aes(group=repetition),colour='grey75',alpha=.5)+
 geom_boxplot(aes(fill=variant),width=.45,outlier.shape=NA,alpha=.55)+
 geom_point(position=position_jitter(width=.055,height=0,seed=42),alpha=.7,size=1.3)+
 scale_fill_manual(values=c('#56B4E9','#E69F00','#009E73'))+
 labs(x=NULL,y='RMSE on 32 hidden Y values',
      title='TabICL: 50 independent datasets and MCAR masks',
      subtitle='160 rows; Y-only 20% MCAR; fixed fine-tuning budget',
      caption='Lines connect the same repetition. Lower is better.')+
 theme_minimal(base_size=12)+theme(legend.position='none')
ggsave(file.path(folder,'rmse.png'),p,width=8,height=5,dpi=180)
z<-result$paired
z$comparison<-factor(paste(z$second,'minus',z$first),levels=rev(paste(z$second,'minus',z$first)),
 labels=rev(c('Fine-tuned minus pretrained','Sequential fine-tuned minus pretrained',
              'Sequential fine-tuned minus fine-tuned')))
p<-ggplot(z,aes(mean_rmse_difference,comparison))+
 geom_vline(xintercept=0,linetype='dashed',colour='grey55')+
 geom_segment(aes(x=ci_low,xend=ci_high,yend=comparison),linewidth=.8,colour='#0072B2')+
 geom_point(size=2.7,colour='#0072B2')+
 labs(x='Paired RMSE difference (negative favors second method)',y=NULL,
 title='Paired differences with 95% Monte Carlo intervals')+theme_minimal(base_size=11)
ggsave(file.path(folder,'paired.png'),p,width=9,height=3.6,dpi=180)
rmarkdown::render(file.path(folder,'report.Rmd'),quiet=TRUE)
print(result$summary,row.names=FALSE)
print(result$paired,row.names=FALSE)
