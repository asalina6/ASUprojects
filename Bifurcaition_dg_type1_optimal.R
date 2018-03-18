  rm(list = ls(all = TRUE))
  set.seed(2)
  #require(sfsmisc)
  ##################################################################################
  # Data
  ##################################################################################
  setwd("C:/Users/Armando/Dropbox/ArmandoSalinas/Armando-Research/April/Ants/Numerical/R")
  #setwd("~/Dropbox/ArmandoSalinas/Armando-Research/April/Ants/Numerical/R")
  
  adat = read.csv("ratios_2.csv",header=T,sep=",")
  
  x_coords = adat$X
  N_observed = adat$Population
  
  #x_coords=x_coords[-length(x_coords)]
  #N_observed=N_observed[-length(N_observed)]
  
  
  ##################################################################################
  # functions
  ##################################################################################
  function_f=function(x, vparameters)
    {
    with(as.list(vparameters),{foutput = (egg*dg*x^2)/(2*(-x^2*(egg*dg+2*dg-2*dw)+2*x*(dg-2*dw)+2*dw))+.5})
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
  dw=.0025;
  dgvector=seq(0,.0025, .0001);
  dg=numeric(0)
  vJ=numeric(0)
  increment=.00001
  digits=5
  x=seq(.00934,.501,increment)
  isPos=T;
  

  
  
  Nstar=seq(0,length(dgvector)-1)
  Xstar=seq(0,length(dgvector)-1)
  
  #Save the estimated parameter best fits
  BNbestvector=seq(0,length(dgvector)-1)
  BGbestvector=seq(0,length(dgvector)-1)
  deltaNbestvector=seq(0,length(dgvector)-1)
  deltaGbestvector=seq(0,length(dgvector)-1)
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
  
  for( i in 1:length(dgvector))
  {
    if (i%%2==0){
      cat("Doing iteration ",i," out of ",length(dgvector)-1,"\n")  
    }
    #maybe comment these out
    Bn_best= numeric(0)
    Bg_best= numeric(0)
    deltan_best = numeric(0)
    deltag_best = numeric(0)
    
  
    N_predicted=seq(0,length(x_coords)-1)
    
    
    
    
    dg=dgvector[i]
    setparameters=c(egg,dw,dg)
    fout = function_f(x,setparameters)
    gout = function_g(x,setparameters)
      
    #for(l in 1:length(gout))
     # {
      #  if(gout[i]<0)
       # {
        #  fout[i]=0
        #}
      #}
    
    best_poissonneglog_likelihood_so_far = 1e10 
    niter=5000
    #niter = 10000

      for( iter in 1:niter)
      {
        if (iter%%100==0)
          {
          cat("Doing iteration ",iter," out of ",niter,"of cycle", i,"\n")  
          }

        
        ###############################################################################
        # randomly sample normally
        ###############################################################################
        Bn   = runif(1,53.465,144.436)
        Bg = runif(1,10.114,22.738)
        deltan = runif(1, 0.001,.002)
        deltag = runif(1, 1.59, 1.715)
        
        vparameters = c(Bn=Bn,Bg=Bg,deltan=deltan,deltag=deltag)
        
        
        N_out=function_N(x,fout,vparameters)
        
        
        
        if(is.na(min(N_out)) )
        {
          isPos=F;
        }
        
        #for(y in 1:length(x))
         # {
          #  if(is.na(N_out[y])|| N_out[y]<0)
           # {
            #  isPos=F;
            #}
          #}
        
        for(z in 1:length(x_coords))
          {
            N_predicted[z]=N_out[indices_vector[z]]
          }
        
        if(isPos)
          {
          ###############################################################################
            if (length(N_predicted)==length(N_observed) &!is.na(sum(N_predicted)))
              {
          
              ########################################################################### 
              # calculate the Neg Log Likelihood statistic
              ########################################################################### 
          
              poissonneglog_likelihood = sum(-(N_observed*log(N_predicted)-N_predicted))
          
    
          
              if (poissonneglog_likelihood < best_poissonneglog_likelihood_so_far)
                {
                  best_poissonneglog_likelihood_so_far = poissonneglog_likelihood
                  Bn_best = Bn
                  Bg_best = Bg
                  deltan_best = deltan
                  deltag_best = deltag
                } #Closes if best statement
              }#closes the check to make sure vectors of predicted and observed same length
          }#closes the ifPos statement
        if(!isPos)#Now checks if its not positive.
          {
            #if (length(N_predicted)==length(N_observed)&!is.na(sum(N_predicted)))
             # {
                Bn_best = 0
                Bg_best = 0
                deltan_best = 0
                deltag_best = 0
              # } #Closes if best statement
          }#closes the else statement
      }#This ends the iter out of niter loop
    
    
    if(isPos)
    { #opens isPos
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
      
        if(round(Nout[j])==round(gout[j])) #used to be 2
        {
          vJ=j
        }
      }
      Nstar[i]=Nout[vJ]
      Xstar[i]=x[vJ]
    }#ends isPos
    if(!isPos)
    {
      Nstar[i]=0
      Xstar[i]=0
    }
  }#This ends thed dg vector loop
  text_main = paste("Bifurcation Type 1 with Sensitivity: dg")
  mult.fig(6,main=text_main,oma=c(1,2,4,1))
  
  plot(dgvector,Nstar, main="Bifurcation Type 1: dg vs N*",xlab="dg",ylab="N*")
  plot(dgvector,Xstar,main="Bifurcation Type 1: dg vs X*",xlab="dg",ylab="X*")
  plot(dgvector,BNbestvector, main=bquote(plain("Bifurcation Type 1: dg vs Optimal fit ")*beta*plain("n")*plain(" value")),xlab="dg",ylab=expression(paste(beta,"n")))
  plot(dgvector,BGbestvector, main=bquote(plain("Bifurcation Type 1: dg vs Optimal fit ")*beta*plain("g")*plain(" value")),xlab="dg",ylab=expression(paste(beta,"g")))
  plot(dgvector,deltaGbestvector, main=bquote(plain("Bifurcation Type 1: dg vs Optimal fit ")*delta*plain("n")*plain(" value")),xlab="\n dg",ylab=expression(paste(delta,"n")))
  plot(dgvector,deltaNbestvector, main=bquote(plain("Bifurcation Type 1: dg vs Optimal fit ")*delta*plain("g")*plain(" value")),xlab="dg",ylab=expression(paste(delta,"g")))
  
  dgvector
  Nstar
  Xstar
  BNbestvector
  BGbestvector
  deltaGbestvector
  deltaNbestvector
  