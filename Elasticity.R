rm(list=ls(all=T))

eggsens=-0.00446801;
dgsens=-0.00425752;
dwsens=0.00425752;
bgsens=-0.629723;
bnsens=0.629723;
deltagsens=-2.11186;
deltansens=0.00543636;

vals=c(eggsens,
       dgsens,
       dwsens,
       bgsens,
       bnsens,
       deltagsens,
       deltansens)

barplot(vals,main="Elasticity of Parameters (Optimal Survival)",col="red",names.arg=c("e","dg","dw",expression(paste(beta,"g")),expression(paste(beta,"n")),expression(paste(delta,"g")),expression(paste(delta,"n"))))


eggsens=-0.00118427;
dgsens=-0.00115593;
dwsens=0.00115593;
bgsens=-0.629723;
bnsens=0.629723;
deltagsens=-1.48525;
deltansens=0.00232322;
vals=c(eggsens,
       dgsens,
       dwsens,
       bgsens,
       bnsens,
       deltagsens,
       deltansens)

barplot(vals,main="Elasticity of Parameters (Best Fit)",col="red",names.arg=c("e","dg","dw",expression(paste(beta,"g")),expression(paste(beta,"n")),expression(paste(delta,"g")),expression(paste(delta,"n"))))

