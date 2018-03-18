  rm(list = ls(all = TRUE))
  set.seed(1)
  ##################################################################################
  # Data
  ##################################################################################
  setwd("C:/Users/Armando/Dropbox/ArmandoSalinas/Armando-Research/April/Ants/Numerical/R")
  #setwd("~/Dropbox/ArmandoSalinas/Armando-Research/April/Ants/Numerical/R")
  
  adat = read.csv("ratios_2.csv",header=T,sep=",")
  
  x_coords = adat$X
  N_observed = adat$Population
  
  
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
  ########################################
  #Initial set up
  #########################################
  egg=2.42
  dg=.001
  dwvector=seq(.0025,.005,.0001);
  dw=numeric(0)
  vJ=numeric(0)
  increment=.00001
  digits=5
  x=seq(.00934,.501,increment)
  
  
  
  Nstar=seq(0,length(dwvector)-1)
  Xstar=seq(0,length(dwvector)-1)
  
  #Save the estimated parameter best fits
  BNbestvector=seq(0,length(dwvector)-1)
  BGbestvector=seq(0,length(dwvector)-1)
  deltaNbestvector=seq(0,length(dwvector)-1)
  deltaGbestvector=seq(0,length(dwvector)-1)
  #finding indices
  indices_vector=rep(0,length(x_coords))
  if(1)
  {
    indices_vector=seq(0,length(x_coords)-1)
    for(m in 1:length(x_coords))
    {
      indices_vector[m]=match(round(x_coords[m],digits),round(x,digits))
    }
  }  
  
  for( i in 1:length(dwvector))
  {
    if (i%%2==0){
      cat("Doing iteration ",i," out of ",length(dwvector)-1,"\n")  
    }
    #maybe comment these out
    Bn_best= numeric(0)
    Bg_best= numeric(0)
    deltan_best = numeric(0)
    deltag_best = numeric(0)
    
  
    N_predicted=seq(0,length(x_coords)-1)
    
    
    
    
    dw=dwvector[i]
    setparameters=c(egg,dw,dg)
    fout = function_f(x,setparameters)
    gout = function_g(x,setparameters)
      
    for(l in 1:length(gout))
      {
        if(gout[i]<0)
        {
          fout[i]=0
        }
      }
      
      
    #vbetan = numeric(0)
    #vbetag = numeric(0)
    #vdeltan = numeric(0)
    #vdeltag = numeric(0)
      
    vpoissonneglog_likelihood = numeric(0) 
      
    best_poissonneglog_likelihood_so_far = 1e10 
    #vbest_poissonneglog_likelihood_fit_prediction = rep(0,length(x_coords))
    #niter=1000
    #niter=10000
    niter=5000
    #niter=2 
      for( iter in 1:niter)
      {
        if (iter%%100==0){
          cat("Doing iteration ",iter," out of ",niter,"of cycle", i,"\n")  
        }
        ##############################################################
        #Save the best data
        ############################################################
        #Bn_best = numeric(0)
        #Bg_best =  numeric(0)
        #deltan_best =  numeric(0)
        #deltag_best =  numeric(0)
        
        ###############################################################################
        # randomly sample normally
        ###############################################################################
        Bn   = runif(1,53.465,144.436)
        Bg = runif(1,10.114,22.738)
        deltan = runif(1, 0.001,.002)
        deltag = runif(1, 1.59, 1.715)
        
        vparameters = c(Bn=Bn,Bg=Bg,deltan=deltan,deltag=deltag)
        
        
        N_out=function_N(x,fout,vparameters)
        
        for(z in 1:length(x_coords))
        {
          N_predicted[z]=N_out[indices_vector[z]]
        }
        
        ###############################################################################
        if (length(N_predicted)==length(N_observed)
            &!is.na(sum(N_predicted)))
          {
          
          ########################################################################### 
          # calculate the Neg Log Likelihood statistic
          ########################################################################### 
          
          poissonneglog_likelihood = sum(-(N_observed*log(N_predicted)-N_predicted))
          
          
          if (poissonneglog_likelihood<best_poissonneglog_likelihood_so_far)
            {
            best_poissonneglog_likelihood_so_far = poissonneglog_likelihood
            #vbest_poissonneglog_likelihood_fit_prediction = N_predicted
            
            
            Bn_best = Bn
            Bg_best = Bg
            deltan_best = deltan
            deltag_best = deltag
           } #Closes if best statement
        
        }#closes the check to make sure vectors of predicted and observed same length
      }#This ends the iter out of niter loop
    BNbestvector[i]=Bn_best
    BGbestvector[i]=Bg_best
    deltaNbestvector[i]=deltan_best
    deltaGbestvector[i]=deltag_best
    
    
    
    vparameters = c(Bn=Bn_best,Bg=Bg_best,deltan=deltan_best,deltag=deltag_best,egg=egg,dw=dw,dg=dg)
    gout=function_g(x,vparameters)
    fout=function_f(x,vparameters)
    Nout=function_N(x,fout,vparameters)
    
    for(j in 1:length(x))
    {
      
      if(round(Nout[j],2)==round(gout[j],2)) #used to be 2
      {
        vJ=j
      }
    }
    Nstar[i]=Nout[vJ]
    Xstar[i]=x[vJ]
    
  }#This ends the dw vector loop
  text_main = paste("Bifurcation Type 1 with Sensitivity: dw")
  mult.fig(6,main=text_main,oma=c(1,2,4,1))
  
  plot(dwvector,Nstar, main="Bifurcation Type 1: dw vs N*",xlab="dw",ylab="N*")
  plot(dwvector,Xstar,main="Bifurcation Type 1: dw vs X*",xlab="dw",ylab="X*")
  plot(dwvector,BNbestvector, main=bquote(plain("Bifurcation Type 1: dw vs Optimal fit ")*beta*plain("n")*plain(" value")),xlab="dw",ylab=expression(paste(beta,"n")))
  plot(dwvector,BGbestvector, main=bquote(plain("Bifurcation Type 1: dw vs Optimal fit ")*beta*plain("g")*plain(" value")),xlab="dw",ylab=expression(paste(beta,"g")))
  plot(dwvector,deltaGbestvector, main=bquote(plain("Bifurcation Type 1: dw vs Optimal fit ")*delta*plain("n")*plain(" value")),xlab="\n dw",ylab=expression(paste(delta,"n")))
  plot(dwvector,deltaNbestvector, main=bquote(plain("Bifurcation Type 1: dw vs Optimal fit ")*delta*plain("g")*plain(" value")),xlab="dw",ylab=expression(paste(delta,"g")))
  
  Nstar
  Xstar
  BNbestvector
  BGbestvector
  deltaGbestvector
  deltaNbestvector
  
  
  