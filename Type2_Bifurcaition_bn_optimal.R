rm(list = ls(all = TRUE))
set.seed(1734)
##################################################################################
# functions
##################################################################################
function_f=function(x, vparameters){
  with(as.list(vparameters),{
    
    foutput = (egg*dg*x^2)/(2*(-x^2*(egg*dg+2*dg-2*dw)+2*x*(dg-2*dw)+2*dw))+.5
  })
}

function_g=function(x,vparameters){
  with(as.list(vparameters),{
    goutput=((1-x)*egg*x)/(-x^2*(egg*dg+2*dg-2*dw)+2*x*(dg-2*dw)+2*dw)
    
  })
}

function_N=function(x,fx,vparameters){
  with(as.list(vparameters),{
    Noutput=((Bn*(1-fx)*x^(-deltag))/(Bg*fx))^(1/(deltag-deltan))
  })
}
######################################3
#parameters
#########################################
################################################################################
egg=2.42;
dw=.0025
dg=.001
bnvector=seq(53.465,144.436,3.63)
Bg=10.114;
deltan=.002;
deltag=1.59;
vJ=numeric(0)
increment=.000001
digits=6
x=seq(.00934,.501,increment)

Nstar=seq(0,length(bnvector)-1)
Xstar=seq(0,length(bnvector)-1)
for( i in 1:length(bnvector))
{
  
  if (i%%2==0){
    cat("Doing iteration ",i," out of ",length(bnvector)-1,"\n")  
    }
  Bn=bnvector[i]
  vparameters = c(Bn=Bn,Bg=Bg,deltan=deltan,deltag=deltag)

  #################################################################3
 
 
  #############################################
  gout=function_g(x,vparameters)
  fout=function_f(x,vparameters)
  Nout=function_N(x,fout,vparameters)

  #plot(x,Nout,type="l")
  #lines(x,gout)
  for(j in 1:length(x))
  {
  
    if(round(Nout[j],3)==round(gout[j],3))
    {
    #print(x[j])
    #print(j)
      vJ=j
    #append(vJ,j)
    }
  }
  #cat("This is Nout:",Nout[vJ])
  Nstar[i]=Nout[vJ]
  Xstar[i]=x[vJ]
}
text_main = paste("Bifurcation Type 2")
mult.fig(2,main=text_main,oma=c(1,2,4,1))
plot(bnvector,Nstar,xlab=bquote(beta*plain("n values from [53.465,144.436]")),ylab="N*-Equilibirum",main=bquote(plain("Bifurcation Diagram: ")*beta*plain("n vs N*")))
plot(bnvector,Xstar,xlab=bquote(beta*plain("n values from [53.465,144.436]")),ylab="X*-Equilibirum",main=bquote(plain("Bifurcation Diagram: ")*beta*plain("n vs X*")))

bnvector
Nstar
Xstar
