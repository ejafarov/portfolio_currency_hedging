#DATA IMPORT#----
#importing data series
currency_returns<-read.csv("EURUSD.csv", row.names=1)
currency_returns.xts<-as.xts(currency_returns,dateFormat = "Date")
index_returns_oc<-read.csv("index_returns_oc.csv", row.names=1)
index_returns_oc.xts<-as.xts(index_returns_oc,dateFormat = "Date")
index_returns_uh<-read.csv("index_returns_uh.csv", row.names=1)
index_returns_uh.xts<-as.xts(index_returns_uh,dateFormat = "Date")
index_returns_he<-read.csv("index_returns_he.csv", row.names=1)
index_returns_he.xts<-as.xts(index_returns_he,dateFormat = "Date")
index_returns_fh<-read.csv("index_returns_fh.csv", row.names=1)
index_returns_fh.xts<-as.xts(index_returns_fh,dateFormat = "Date")

asset_classes<-read.csv("asset_classes.csv", stringsAsFactors = FALSE)%>%
  as.tibble

#combining unique columns
index_returns_all<-cbind(index_returns_uh,index_returns_he)
index_returns_all<-index_returns_all[, !duplicated(colnames(index_returns_all))]
index_returns_all<-cbind(index_returns_all,currency_returns)
index_returns_all.xts<-as.xts(index_returns_all,dateFormat = "Date")

colnames(index_returns_all)
index_returns_all%>%
  as.tibble%>%
  gather(key="asset",value="return")%>%
  ggplot()+
  geom_histogram(aes(return),stat="bin")+
  facet_wrap(~asset, scales = "free")

#ANALYZING UNHEDGED VS. HEDGE PAIRS####
#construct unhedged-hedged pairs
index_returns_uhvsh<-index_returns_all%>%
  select(USTREUR,USTREURH,USIGEUR,USIGEURH,USHYEUR,USHYEURH,EMSOEUR,EMSOEURH,USLCEUR,USLCEURH,USSCEUR,USSCEURH,EMEQEUR,EMEQEURH)
index_returns_uhvsh.xts<-as.xts(index_returns_uhvsh,dateFormat = "Date")

#statistics and ratio analysis
index_returns_uhvsh.xts%>%
  table.AnnualizedReturns%>%
  as.tibble%>%
  rownames_to_column(var="measure")%>%
  gather(key="asset",value="value",-measure)%>%
  filter(measure=="Annualized Sharpe (Rf=0%)")%>%
  left_join(asset_classes,by="asset")%>%
  arrange(segment)%>%
  ggplot()+
    geom_bar(aes(asset,value,fill=hedge), stat="identity")+
    facet_wrap(segment~class,scale='free',ncol = 4)+
    ggtitle("Sharpe ratio of hedged and unhedged assets by class 1999-2017")+
    theme_minimal(base_size = 16)+
    theme(plot.title = element_text(hjust=0.5))


#ROLLING CALCULATIONS#----
#preparing data for rolling calculations
index_returns_uhvsh_rs<-index_returns_uhvsh.xts%>%
  rollapply(width=36,by.column=TRUE,function(x) SharpeRatio.annualized(x))%>%
  na.omit%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  as.tibble%>%
  mutate(date=as.Date(date))%>%
  gather(key="asset","value",-date)%>%
  left_join(asset_classes,by="asset")

#boxplot representation
index_returns_uhvsh_rs%>%
  ggplot()+
    geom_boxplot(aes(x=asset,y=value,color=hedge),size=1)+
    facet_wrap(segment~class,scale="free_x",ncol=4)+
    theme_gray(base_size=14)

#histogram reresentation
index_returns_uhvsh_rs%>%
  ggplot()+
  geom_histogram(aes(value,fill=hedge),size=1,color="gray",stat="bin")+
  facet_wrap(segment~class,scale="free_x",ncol=4)+
  theme_gray(base_size=14)

#violin reresentation
index_returns_uhvsh_rs%>%
  ggplot()+
  geom_violin(aes(x=asset, y=value,fill=hedge))+
  facet_wrap(segment~class,scale="free_x",ncol=4)+
  theme_gray(base_size=14)
  
#line representation
index_returns_uhvsh_rs%>%
  ggplot()+
    geom_line(aes(x=date,y=value,color=hedge),size=2)+
    facet_wrap(segment~class,scale="free_x",ncol=4)+
    theme_gray(base_size=14)

#calculating and plotting rolling volatility
asset_name="USHYEUR"
win=36
asset_name_h=paste(asset_name,"H",sep="")

rsduh<-index_returns_uhvsh.xts[,asset_name]%>%
  rollapply(win,function(x) StdDev.annualized(x))%>%
  na.omit%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  mutate(date=as.Date(date))%>%
  as.tibble

rsdh<-index_returns_uhvsh.xts[,asset_name_h]%>%
  rollapply(win,function(x) StdDev.annualized(x))%>%
  na.omit%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  mutate(date=as.Date(date))%>%
  as.tibble

ggplot()+
  geom_line(data=rsduh, aes(x=date,y=eval(parse(text = asset_name))),color="black",size=2)+
  geom_line(data=rsdh, aes(x=date,y=eval(parse(text = asset_name_h))),color="red",size=2)+
  ggtitle(paste(win,"moth rolling annualized standard deviation for",asset_name,"vs.",asset_name_h))+
  labs(x="Date",y="Standard deviation")+
  theme_minimal(base_size = 16)+
  theme(plot.title = element_text(hjust=0.5))



#calculating and plotting rolling Sharpe
rsruh<-index_returns_uhvsh.xts[,asset_name]%>%
  rollapply(win,function(x) SharpeRatio.annualized(x))%>%
  na.omit%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  mutate(date=as.Date(date))%>%
  as.tibble

rsrh<-index_returns_uhvsh.xts[,asset_name_h]%>%
  rollapply(win,function(x) SharpeRatio.annualized(x))%>%
  na.omit%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  mutate(date=as.Date(date))%>%
  as.tibble

left_join(rsruh,rsrh, by="date")%>%
  gather(key="asset",value="sharpe",-date)%>%
  left_join(asset_classes,by="asset")%>%
  t.test(USHYEUR,USHYEURH,paired=TRUE,conf.level=0.95)
  
  ggplot()+
  geom_boxplot(aes(x=asset,sharpe))+
  facet_wrap(~class)

ggplot()+
  geom_line(data=rsruh, aes(x=date,y=eval(parse(text = asset_name))),color="black")+
  geom_line(data=rsrh, aes(x=date,y=eval(parse(text = asset_name_h))),color="red")


#correlation plots

write.csv(table.Correlation(cbind(index_returns_uhvsh.xts,currency_returns.xts),cbind(index_returns_uhvsh.xts,currency_returns.xts)),"correlation.csv")

chart.Correlation(cbind(index_returns_uhvsh.xts,currency_returns.xts), method = c("pearson"))
chart.Correlation(cbind(index_returns_uhvsh.xts,currency_returns.xts), method = c("kendall"))
chart.Correlation(index_returns_uhvsh.xts, method = c("spearman"))

corrplot(cor(cbind(index_returns_uhvsh.xts,currency_returns.xts)), order = "hclust", addrect=6, method="circle", diag=TRUE)



#correlation analysis

table.Correlation(index_returns_all.xts,index_returns_all.xts)%>%
  rownames_to_column%>%
  as.tibble%>%
  'colnames<-'(c("pair","corr","p","lower","upper"))%>%
  filter(p != 0, lower>0.85)%>%
  transmute(pair,corr,p=round(p,4),lower,upper)%>%
  arrange(desc(corr))%>%
  head(20)
  
  length(colnames(index_returns_all.xts))

plot(currency_returns.xts[,"EURUSD"]) 
plot(index_returns_all.xts[,"EZTREUR"])
charts.PerformanceSummary(cbind(currency_returns.xts[,"EURUSD"],index_returns_all.xts[,"USTREUR"]))
charts.PerformanceSummary(cbind(index_returns_all.xts[,"USTREURH"],index_returns_all.xts[,"EZTREUR"]))
  
pdf("correl_charts.pdf")
par(mar=c(2,2,2,2))
  
par(mfrow=c(1,1))
chart.Correlation(index_returns_all.xts, method = c("pearson"))
chart.Correlation(index_returns_all.xts, method = c("kendall"))
chart.Correlation(index_returns_all.xts, method = c("spearman"))
corrplot(cor(index_returns_all.xts), order = "hclust", addrect=8, method="circle", diag=TRUE)
chart.RiskReturnScatter(index_returns_all.xts)  

chart.SnailTrail(currency_returns.xts, width=36, stepsize=3, add.sharpe = FALSE, color="blue")
chart.SnailTrail(index_returns_all.xts[,"EURUSD"], width=36, stepsize=3, add.sharpe = FALSE, color="blue")
hist(index_returns_all.xts[,"EURUSD"])
#creating fancy histogram
index_returns_all.xts%>%
  ggplot(aes(EURUSD))+ 
  geom_histogram(breaks=seq(-0.20, 0.2, by=0.004), col="gray", aes(fill=..count..))+
  scale_fill_gradient("Count", low="gray", high="black")+
  geom_density(col="gray", size=1)+
  stat_function(fun=dnorm, color="red", 
                args=list(mean=mean(index_returns_all.xts[,"EURUSD"]),sd=sd(index_returns_all.xts[,"EURUSD"])),
                size=1)+
  ggtitle("EUR/USD exchange rate histogram for 1999-2017")+
  theme(plot.title = element_text(hjust = 0.5))
  
  
chart.Boxplot(index_returns_uhvsh.xts, sort.by = c("NULL"))

#scatter plot with directed lines
index_returns_uhvsh_summary<-index_returns_uhvsh%>%
  rownames_to_column(var="date")%>%
  mutate(date=as.Date(date))%>%
  as.tibble%>%
  gather(key="asset",value="return",-date)%>%
  group_by(asset)%>%
  summarize(mean=mean(return),sd=sd(return))%>%
  mutate(mean=mean*12,sd=sd*12^0.5)
  

index_returns_uhvsh_summary_direct<-index_returns_uhvsh_summary%>%
  gather(key="statistic",value="value",-asset)%>%
  mutate(class=substr(asset,1,4))%>%
  mutate(asset=ifelse(grepl("EURH",asset),"hedged","unhedged"))%>%
  mutate(statistic=paste(statistic,asset,sep="_"))%>%
  select(-asset)%>%
  spread(key=statistic,value=value)
  
  ggplot()+
    geom_point(data=index_returns_uhvsh_summary, aes(x=sd,y=mean),size=5,color="red")+
    geom_segment(data=index_returns_uhvsh_summary_direct, 
                 aes(x=sd_unhedged,y=mean_unhedged,xend=sd_hedged,yend=mean_hedged),
                 arrow=arrow(angle=20, length=unit(0.3,"inches"),ends="last", type = "closed"), 
                 size=2, color="blue", linetype="solid",alpha=1)+
    geom_label(data=index_returns_uhvsh_summary, 
               mapping=aes(x=sd,y=mean, label=asset,  hjust=1, vjust=0), size=5)+
    ggtitle("Mean-Variance of unhedged vs. hedged asset classes")+
    coord_cartesian(xlim = c(0,0.24), ylim=c(0,0.14),expand = FALSE)+
    scale_x_continuous(labels= function(x){ paste0(x*100, "%")}, breaks=seq(from=0,to=0.24,by=0.02))+
    scale_y_continuous(labels= function(x){ paste0(x*100, "%")},breaks=seq(from=0,to=0.14,by=0.02))+
    theme(plot.title = element_text(face="bold", size=22, hjust=0.5))+
    theme(axis.title = element_text(face="bold", size=18))+
    theme(axis.text = element_text(face="plain", size=14))


#ploting individual rolling correlation
  #investingating EMEQ and EURUSD relationship
chart.RollingCorrelation(index_returns_uhvsh.xts[,"EMEQEURH"],currency_returns.xts[,"EURUSD"],width=12)

#plotting rolling correlations for all asset combinations  
par(mfrow=c(5,5))
for (k in c(12,36,60)){  
  for(i in c(1:21)){
    for (j in c((i+1):22)){
      chart.RollingCorrelation(index_returns_all.xts[,i],index_returns_all.xts[,j],width=k) 
    }
  }
}
dev.off()


#make a gathered tibble  
index_returns_container<-index_returns_selected

index_returns_container_gathered<-index_returns_container%>%
    rownames_to_column(var="date")%>%
    mutate(date=as.Date(date))%>%
    as.tibble%>%
    gather(key="asset",value="return",-date)%>%
    arrange(date)%>%
    mutate(class=substr(asset,1,4))
    
index_returns_container_gathered%>%
    ggplot(aes(x=date,y=return))+
    geom_line()+
    geom_smooth()+
    facet_wrap(~asset)
  
index_returns_container_gathered%>%
  ggplot()+
  geom_boxplot(aes(x=date,y=return, group=asset), outlier.alpha = 0.2)+
  facet_wrap(~class)+
  theme(axis.text.x=element_blank())

index_returns_container_gathered%>%
  ggplot()+
  geom_line(aes(x=date,y=return))+
  facet_wrap(~asset)

index_returns_container_gathered%>%
  ggplot(aes(return))+
  geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("count", low="gray", high="black")+
  facet_wrap(~asset)
  
#chart individual hedged and unhedged returns
index_returns_container%>%
  select(EMEQEUR,EMEQEURH)%>%
  rownames_to_column(var="date")%>%
  as.tibble%>%
  mutate(date=as.Date(date))%>%
  ggplot()+
  geom_line(aes(x=date,y=EMEQEUR),color="blue")+
  geom_line(aes(x=date,y=EMEQEURH),color="red")


#PORTFOLIO CONSTRUCTION####
#define strategies
strategies=c("unhedged","hedged","fi_hedged")

#put returns into container
returns_container<-index_returns_all
returns_container.mat<-as.matrix(returns_container)
returns_container.xts<-as.xts(returns_container, dateFormat = "Date")

asset_spec<-list()
asset_spec<-list(names=list(unhedged=colnames(index_returns_uh),
                hedged=colnames(index_returns_he),
                fi_hedged=colnames(index_returns_fh)
              ),
            counts=list(unhedged=length(asset_spec$names$unhedged),
                hedged=length(asset_spec$names$unhedged),
                hedged=length(asset_spec$names$fi_hedged)
              )
            )

#specifying generic portfolio
port_spec<-list(unhedged=portfolio.spec(assets=asset_spec$names$unhedged),
                hedged=portfolio.spec(assets=asset_spec$names$hedged),
                fi_hedged=portfolio.spec(assets=asset_spec$names$fi_hedged)
                )

port_spec<-port_spec%>%
  lapply(function(x) add.constraint(x, type="long_only"))%>%
  lapply(function(x) add.constraint(x, type="weight_sum", min_sum=0.99, max_sum=1.01))%>%
  lapply(function(x) add.constraint(x, type="diversification", div_target=0.8))%>%
  lapply(function(x) add.constraint(x, type="group", groups=list(Bonds=c(1:9),Equities=c(10:14)),
                                                    group_min=c(0.55, 0.35),
                                                    group_max=c(0.65, 0.45))) 

#define max Sharpe portfolio
port_spec_maxSharpe<-port_spec%>%
  lapply(function(x) add.objective(x, type="return", name="mean"))%>%
  lapply(function(x) add.objective(x, type="risk", name="StdDev"))

#define min ETL portfolio
port_spec_minETL<-port_spec%>%
  lapply(function(x) add.objective(x, type="return", name="mean"))%>%
  lapply(function(x) add.objective(x, type="risk", name="ETL", arguments = list(p=0.95)))


  
comb_port_spec_maxSharpe<-combine.portfolios(port_spec_maxSharpe)
comb_port_spec_minETL<-combine.portfolios(port_spec_minETL)


#START MULTI-CORE CLUSTER####
cl <- makeCluster(7)
registerDoSNOW(cl)

clusterExport(cl,"returns_container.xts")
clusterExport(cl,"returns_container.mat")
clusterExport(cl,"StdDev")
clusterExport(cl, "random_portfolios")
clusterExport(cl, "meanvar.efficient.frontier")


#GENERATE FEASIBLE PORTFOLIOS####
#calculate asset metrics
assets_sd=NULL
assets_mean=NULL
assets_etl=NULL
assets_var=NULL
for (i in c(1:length(colnames(returns_container)))){
  assets_sd[i]=sd(returns_container.mat[,i])*(12^0.5)
  assets_mean[i]=mean(returns_container.mat[,i])*12
  assets_etl[i]=ETL(returns_container.mat[,i], p=0.95, method="historical")
  assets_var[i]=VaR(returns_container.mat[,i], p=0.95, method="historical")
}

#put into df for chart lables
assets_data_sd<-cbind(assets_mean,assets_sd)%>%
  as.data.frame%>%
  'rownames<-'(colnames(returns_container))

assets_data_ETL<-cbind(assets_mean,assets_etl)%>%
  as.data.frame%>%
  'rownames<-'(colnames(returns_container))

#generate random portfolios for the portfolio


set.seed(round(runif(1)*as.numeric(Sys.time())/as.numeric(Sys.Date()),0))

#rp generation for combined portfolios (parallelized)
rp_all<-list()
for (rp_method in c("sample", "simplex", "grid")){
  clusterExport(cl, "rp_method")
  rp_all$weights[[rp_method]]<-parLapply(cl, comb_port_spec_maxSharpe, function(x) random_portfolios(x, permutations=5000, rp_method=rp_method))
}

rp_all$weights$all<-list(unhedged=rbind(rp_all$weights$sample$unhedged, 
                                        rp_all$weights$simplex$unhedged,
                                        rp_all$weights$grid$unhedged
                                        ),
                         hedged=rbind(rp_all$weights$sample$hedged, 
                                        rp_all$weights$simplex$hedged,
                                        rp_all$weights$grid$hedged
                                        ),
                         fi_hedged=rbind(rp_all$weights$sample$fi_hedged, 
                                      rp_all$weights$simplex$fi_hedged,
                                      rp_all$weights$grid$fi_hedged
                                      )
                        )

#to workoout if needed
rp_all[[rp_method]]<-lapply(rp_all[[rp_method]], setNames, nm=colnames)
colnames(t(comb_port_spec_maxSharpe[["hedged"]]$assets))
lapply(listDF, setNames, nm = new_col_name)
  
#replaced ths with above list or rp
rp_sa <- parLapply(cl, comb_port_spec_maxSharpe, function(x) random_portfolios(x, permutations=10000, rp_method='sample'))
rp_si <- parLapply(cl, comb_port_spec_maxSharpe, function(x) random_portfolios(x, permutations=10000, rp_method='simplex'))
rp_gr <- parLapply(cl, comb_port_spec_maxSharpe, function(x) random_portfolios(x, permutations=10000, rp_method='grid'))

#rp generation for individual portfolios
rp_sa <- random_portfolios(portfolio=comb_port_spec_maxSharpe, permutations=1000, rp_method='sample')
rp_si <- random_portfolios(portfolio=pspec_container, permutations=1000, rp_method='simplex')
rp_gr <- random_portfolios(portfolio=pspec_container, permutations=1000, rp_method='grid')
#colnames(rp_gr)<-colnames(returns_container)


#preparing multicore cluster
clusterExport(cl,"rp_all")
clusterExport(cl,"HHI")
clusterExport(cl,"ETL")
#calling package into the cluster
clusterCall(cl, function() library(magrittr))

for (i in strategies){
  clusterExport(cl,"i")
  rp_all$stats$mean[[i]] <- parRapply(cl, rp_all$weights$all[[i]], function(x) mean(returns_container.mat[,colnames(rp_all$weights$all[[i]])] %*% x))
  rp_all$stats$mean[[i]] <- rp_all$stats$mean[[i]]*12
  rp_all$stats$sd[[i]] <- parRapply(cl, rp_all$weights$all[[i]], function(x) StdDev(returns_container.xts[,colnames(rp_all$weights$all[[i]])], weights=x))
  rp_all$stats$sd[[i]] <- rp_all$stats$sd[[i]] * 12^0.5
  
  rp_all$stats$etl[[i]] <- unlist(
    foreach (j = c(1:dim(rp_all$weights$all[[i]])[1]), .combine = list, .multicombine = TRUE) %dopar% {
      #clusterExport(cl,"j")
      #temp[[j]] <- 
      returns_container.xts%>%
        .[,colnames(rp_all$weights$all[[i]])]%>%
        ETL(weights=rp_all$weights$all[[i]][j,], portfolio_method= 'component')%>%
        .$MES
    }
  )
  
  rp_all$stats$HHI[[i]] <- parRapply(cl, rp_all$weights$all[[i]], function(x) HHI(weights=x))
  
  #putting al stats together
  if(i==strategies[1]){
    rp_all$stats$all<-cbind(rp_all$stats$mean[[i]],
                           rp_all$stats$sd[[i]],
                           rp_all$stats$etl[[i]],
                           rp_all$stats$HHI[[i]])%>%
      as.tibble%>%
      'colnames<-'(c("mean","sd","etl","HHI"))%>%
      mutate(strategy=i)
    
  }else{
    rp_all$stats$all<-rp_all$stats$all%>%
      full_join(cbind(rp_all$stats$mean[[i]],
                           rp_all$stats$sd[[i]],
                           rp_all$stats$etl[[i]],
                           rp_all$stats$HHI[[i]])%>%
          as.tibble%>%
          'colnames<-'(c("mean","sd","etl","HHI"))%>%
          mutate(strategy=i)
      )
  }
  
  #calculating efficient frontier stats
  rp_all$ef_stats[[i]]<-meanvar.efficient.frontier(portfolio=port_spec_maxSharpe[[i]], 
                                                   R=returns_container.xts, 
                                                   n.portfolios = 100,
                                                   risk_aversion = NULL)
  
  
}



#excellent vizuals - boxplots by groups 
#by sd
rp_all$stats$all$group_sd <- cut(rp_all$stats$all$sd, 10)

rp_all$stats$all%>%
  ggplot()+
  geom_boxplot(aes(x=group_sd,y=mean,color=strategy))

#by etl
rp_all$stats$all$group_etl <- cut(rp_all$stats$all$etl, 10)

rp_all$stats$all%>%
  ggplot()+
  geom_boxplot(aes(x=group_etl,y=mean,color=strategy))

rp_all$stats$all%>%
  ggplot()+
  geom_point(aes(x=sd,y=mean,color=strategy), alpha=0.5)+
  facet_wrap(~strategy, ncol=1)


#experiments
rp_all$ef_stats$hedged
ef_stats.tbl<-ef_stats[,c("mean","StdDev")]%>%
  as.tibble%>%
  transmute(mean=mean*12,sd=StdDev*(12^0.5))

#stoping the cluster
stopCluster(cl)

ggplot(data=rp_al_stats.tbl)+
  geom_point(mapping=aes(x=sd, y=mean, color=HHI), alpha=1/10)+
  geom_point(data=assets_data_sd, mapping=aes(x=assets_sd,y=assets_mean), color="black")+
  geom_label(data=assets_data_sd, mapping=aes(x=assets_sd,y=assets_mean), label=asset.names,  hjust=-0.1)+
  geom_line(data=ef_stats.tbl,mapping=aes(x=sd,y=mean), color="red")+
  geom_point(data=ef_stats.tbl,mapping=aes(x=sd,y=mean), color="red")



#aggregating the results with other variants of portfolios (hedged, unhedged, mixed)
ef_stats.tbl<-ef_stats.tbl%>%
  mutate(strategy="partially_hedged")

ef_stats_aggregated.tbl<-ef_stats_aggregated.tbl%>%
  full_join(ef_stats.tbl)

rp_al_stats.tbl<-rp_al_stats.tbl%>%
  mutate(strategy="hedged")

rp_al_stats_aggregate.tbl<-rp_al_stats_aggregate.tbl%>%
  full_join(rp_al_stats.tbl)


ef_stats_aggregated.tbl%>%
  ggplot()+
    geom_point(aes(x=sd,y=mean,color=strategy))+
    geom_line(aes(x=sd,y=mean,color=strategy))

ggplot()+
  geom_point(data=rp_al_stats_aggregate.tbl, aes(x=sd,y=mean,color=strategy), alpha=0.3)+
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 5)))+
  geom_point(data=assets_data_sd, aes(x=assets_sd,y=assets_mean), color="black")+
  geom_label(data=assets_data_sd, aes(x=assets_sd,y=assets_mean), label=asset.names,  hjust=-0.1)
  
  #adding efficient fronteer data, if needed  
  geom_line(data=ef_stats_aggregated.tbl,aes(x=sd,y=mean, color=strategy))+
  geom_point(data=ef_stats_aggregated.tbl,aes(x=sd,y=mean, color=strategy))

  
#NAIVE PORTFOLIOS#### 
port_naive<-list()
for (i in strategies){
  port_naive$ew[[i]]<-Return.portfolio(R=returns_container.xts[,asset_spec$names[[i]]],rebalance_on = "years",verbose=FALSE)
}
  
  port_naive$ew$all<-port_naive$ew%>%
    as.data.frame%>%
    'colnames<-'(strategies)%>%
    as.tibble
  
port_naive$ew$all%>%
  chart.CumReturns(legend.loc = "topleft")  

port_naive$ew$all%>%
  gather(key="strategy",value="return")%>%
  ggplot()+
    geom_boxplot(aes(x=strategy,y=return,fill=strategy))

#OPTIMIZING PORTFOLIO####
Sharpe.opt<-optimize.portfolio(R=returns_container.xts, 
                               portfolio=comb_port_spec_maxSharpe, 
                               maxSR=TRUE, 
                               optimize_method = "DEoptim", 
                               #rp=rp_al,
                               trace=TRUE, 
                               search_size = 20000)

#sampling optimization
Sharpe.opt<-optimize.portfolio.parallel(R=returns_container.xts, 
                               portfolio=comb_port_spec_maxSharpe, 
                               maxSR=TRUE, 
                               optimize_method = "DEoptim", 
                               #rp=rp_al,
                               trace=TRUE, 
                               search_size = 20000,
                               nodes=100)

minETL.opt<-optimize.portfolio.parallel(R=returns_container.xts, 
                                        portfolio=comb_port_spec_minETL, 
                                        maxSR=TRUE, 
                                        optimize_method = "DEoptim", 
                                        #rp=rp_al,
                                        trace=TRUE, 
                                        search_size = 20000,
                                        nodes=100)


Sharpe.opt$elapsed_time
minETL.opt$elapsed_time

saveRDS(Sharpe.opt, "Sharpe.opt.Rds")
saveRDS(minETL.opt, "minETL.opt.Rds")

port_container.opt<-minETL.opt

weights<-data.frame()
for(i in c(1:100)){
  weights<-weights%>%
    rbind(as.data.frame(t(port_container.opt$optimizations[[i]][[2]]$weights)))
}

weights%>%
  gather(key="asset",value="weight")%>%
  ggplot()+
  geom_boxplot(aes(x=asset,y=weight))

weights%>%
  as.tibble%>%
  gather(key="asset",value="weight")%>%
  group_by(asset)%>%
  summarize(mean_weight=mean(weight))%>%
  spread(asset,mean_weight)%>%
  HHI
  

out<-numeric()
for(i in c(1:100)){
  out<-out%>%
    rbind(port_container.opt$optimizations[[i]][[2]]$out)
}
out%>%
  as.tibble%>%
  ggplot()+
    geom_histogram(aes(out))

chart.EfficientFrontier(Sharpe.opt.ef, match.col = "StdDev", main="Efficient fronteer", rf=0.02/12)
chart_ef_weights<-chart.EF.Weights(Sharpe.opt, 
                                   n.portfolios = 50, 
                                   by.groups = FALSE, 
                                   match.col = "StdDev", 
                                   colorset=rainbow(20))
#PORTFOLIO BACKTESTING####
optimization.names<-c("maxSharpe")
rebalancing<-"years"
rfr_level<-0.02/12
freq_level<-12
opt_method<-"DEoptim"
training_set<-60
rolling_window<-60

#setting the clusters for parallel
cl <- makeCluster(detectCores()-1, type = "PSOCK")
registerDoSNOW(cl)
getDoParWorkers()

maxSharpe_opt_RB<-list()

clusterExport(cl, "optimize.portfolio.rebalancing")
clusterExport(cl, "maxSharpe_opt_RB")

system.time({
  for (i in c(1:100)) {
    maxSharpe_opt_RB[[i]]<-optimize.portfolio.rebalancing(R=returns_container.xts, comb_port_spec_maxSharpe, 
                                                          optimize_method=opt_method,
                                                          #rp=rp_al,
                                                          maxSR=TRUE,
                                                          search_size=20000, 
                                                          trace=FALSE, 
                                                          message=TRUE,
                                                          rebalance_on=rebalancing, 
                                                          training_period=training_set,
                                                          rolling_window=rolling_window)
    
  }
})
stopCluster(cl)

saveRDS(maxSharpe_opt_RB, "maxSharpe_opt_RB_pso.Rds")

maxSharpe_opt_RB<-readRDS("maxSharpe_opt_RB.Rds")


maxSharpe_opt_RB_weights<-data.frame()
for(r in c(1:100)){
  for (p in c(1:3)){
    
    maxSharpe_opt_RB_weights<-maxSharpe_opt_RB_weights%>%
      rbind(
        extractWeights(maxSharpe_opt_RB[[r]][[p]])%>%
        as.data.frame%>%
        rownames_to_column(var="date")%>%
        as.tibble%>%
        gather(key="asset",value="weight",-date)%>%
        mutate(date=as.Date(date), strategy=(case_when(p==1 ~"unhedged",
                                                      p==2 ~ "hedged",
                                                      p==3 ~ "fi_hedged")
                                                      ),
                                                      run=r)
      )
  }
}

#plotting boxplots
maxSharpe_opt_RB_weights%>%
  ggplot()+
  geom_boxplot(aes(x=asset,y=weight, fill=strategy))+
  facet_grid(date~strategy, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#plotting histograms
maxSharpe_opt_RB_weights%>%
  ggplot()+
  geom_histogram(aes(weight),bins=50)+
  facet_grid(date~strategy)

#plotting average weights
maxSharpe_opt_RB_weights%>%
  group_by(strategy,date,asset)%>%
  summarise(weight_median=median(weight), weight_mean=mean(weight))%>%
  ggplot()+
  geom_bar(aes(x=asset,y=weight_mean, fill=strategy), stat = "identity")+
  facet_grid(date~strategy, scales = "free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#plotting average stacked weights
maxSharpe_opt_RB_weights%>%
  group_by(strategy,date,asset)%>%
  summarise(weight_median=median(weight), weight_mean=mean(weight))%>%
  ggplot()+
  geom_bar(aes(x=date,y=weight_mean, fill=asset), stat = "identity")+
  facet_wrap(~strategy, scales = "free_x", ncol = 1)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#calculated and plotting HHI index at dates
maxSharpe_opt_RB_weights%>%
  group_by(strategy,date,asset)%>%
  summarise(weight_median=median(weight), weight_mean=mean(weight))%>%
  group_by(date,strategy)%>%
  #summarise(weight_sum=sum(weight_mean))
  summarise(HHI=HHI(weight_mean))%>%
  ggplot()+
  geom_line(aes(x=date,y=HHI, color=strategy))+
  facet_wrap(~strategy, ncol = 1)


#BACKTEST ANALYSIS#### 

#generating returns with rebalancing weigts
rebalancing_returns_maxSharpe<-list()
for (p in c(1:3)){
  rebalancing_returns_maxSharpe[[p]]<-maxSharpe_opt_RB_weights%>%
    mutate(strategy=(case_when(strategy=="unhedged" ~ 1,
                              strategy=="hedged" ~ 2,
                              strategy=="fi_hedged" ~ 3)
                              )
          )%>%
    group_by(strategy,date,asset)%>%
    summarise(weight_mean=mean(weight))%>%
    filter(strategy==p)%>%
    spread(key=asset,value=weight_mean)%>%
    ungroup%>%
    select(-strategy)%>%
    as.data.frame%>%
    column_to_rownames(var="date")%>%
    as.xts(dateFormat = "Date")%>%
    Return.portfolio(R=returns_container.xts,weights=.,rebalance_on = rebalancing,verbose=TRUE)
}



rebalancing_returns_maxSharpe<-cbind(rebalancing_returns_maxSharpe[[1]]$returns,
                                    rebalancing_returns_maxSharpe[[2]]$returns,
                                    rebalancing_returns_maxSharpe[[3]]$returns)%>%
  'colnames<-'(c("unhedged", "hedged", "fi_hedged"))

rebalancing_retuns_all<-rebalancing_retuns_naive%>%
  .["2005-01-31/"]%>%
  cbind(rebalancing_returns_maxSharpe)

chart.CumReturns(rebalancing_retuns_all, legend.loc = "topleft", main="")
chart.Drawdown(rebalancing_retuns_all, legend.loc = "bottomleft", main="")

rebalancing_retuns_all%>%
  table.Stats%>%
  as.data.frame%>%
  rownames_to_column(var="measure")%>%
  as.tibble%>%
  write.table("table.csv", append=TRUE, sep=",", row.names = FALSE)
  
table.CAPM(Ra=rebalancing_retuns_all, Rb=rebalancing_retuns_all$unhedged)%>%
    as.data.frame%>%
    rownames_to_column(var="measure")%>%
    as.tibble%>%
    write.table("table_bmrk.csv", append=TRUE, sep=",", row.names = FALSE)
  
  
  #use as templetes
  table.Stats
  table.DownsideRiskRatio
  table.DrawdownsRatio
  table.DownsideRisk


#extracting weights on rebalancing dates
rebalancing_weights_Sharpe<-extractWeights(Sharpe_opt_RB)


rebalancing_weights_Sharpe[[1]]%>%
  apply.monthly(HHI)%>%
  #{1/.}%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  transmute(date=as.Date(date),HHI=V1)%>%
  ggplot()+
    geom_line(aes(x=date,y=HHI))
  


par(mfrow=c(1,1))
chart.Weights(Sharpe_opt_RB[[1]], colorset=rainbow(14))
chart.Weights(Sharpe_opt_RB[[2]], colorset=rainbow(14))
chart.Weights(Sharpe_opt_RB[[3]], colorset=rainbow(14))


chart.CumReturns(cbind(rebalancing_returns_maxSharpe[[1]]$returns,
                       rebalancing_returns_maxSharpe[[2]]$returns,
                       rebalancing_returns_maxSharpe[[3]]$returns
                      ),
    legend.loc = "topleft"
  )

#putting rebalanced portfolio returns together for comparison
returns_maxSharpe_rb.xts<-rebalancing_returns_maxSharpe[[1]]$returns%>%
  as.data.frame%>%
  rownames_to_column(var="date")%>%
  as.tibble%>%
  transmute(date=as.Date(date),returns_uh=portfolio.returns)%>%
  mutate(returns_he=as.double(rebalancing_returns_maxSharpe[[2]]$returns))%>%
  as.data.frame%>%
  column_to_rownames(var="date")%>%
  as.xts(dateFormat = "Date")
  
  gather(key="strategy",value="return",-date)

returns_maxSharpe_rb%>%
  ggplot()+
    geom_line(aes(x=date,y=return,color=strategy))

table.DownsideRiskRatio(returns_maxSharpe_rb.xts)
