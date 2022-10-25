rh.sd.sidak=function(p,effect,effect.se,df,type="mean",sig=0.05,side="two",digits=3){
  m<-length(p)-rank(p)+1
  p.adj<-1-(1-p)^m
  if(type=="mean"){
    pt<-1-(1-sig)^(1/m)
    if(side=="two"){t<-abs(qt(p=pt/2,df=df))}
    if(side=="one"){t<-abs(qt(p=pt,df=df))}
    ci.l<-effect-t*effect.se
    ci.u<-effect+t*effect.se
  }
  # if(type=="or" | type=="rr"){
  #   pz=sig/m
  #   if(side=="two"){z=abs(qnorm(p=pz/2))}
  #   if(side=="one"){z=abs(qnorm(p=pz))}
  #   ci.l=effect-z*effect.se
  #   ci.u=effect+z*effect.se
  # }
  for(i in 2:max(m)){
    if(p.adj[which(m==i)]>p.adj[which(m==(i-1))]){p.adj[which(m==(i-1))]=p.adj[which(m==i)]}
  }
  round(p.adj,4)
  out<-data.frame(p.adj,ci.adj.l=ci.l,ci.adj.u=ci.u)
  out<-round(out,digits)
  return(out)
}

# example, type="mean"
# p=c(0.217,0.00028,0,0.001,0.024,0.719,0.00033)
# effect=c(16,74,-85,-38,29,5,91)
# effect.se=c(12,16,14,9,12,16,20)
# df=16
# rh.sd.sidak(p,effect,effect.se,df)
