

#Function to find optimal spacing in two-patch model
find_optimal_spacing = function(r1,K1,delta,epsilon,omega,prob_disturb, severity_disturb, gamma,output_time_series=FALSE){
  #parameters
  
  num_runs= 300
  
  #distance = 30
  distance_vector = seq(1,50,1)
  total_pop = vector('numeric',length(distance_vector))
  persistence = NA*vector('numeric',length(distance_vector))
  total_var = vector('numeric',length(distance_vector))
  best_run = NULL
  
  #prob_disturb = 0.15;severity_disturb=0.1
  #delta=0.03
  source('../scripts/two_patch/simulate_model.R')
  for (i in 1:length(distance_vector)){
    set.seed(123)
    distance = distance_vector[i]
    model_output= matrix(0,nrow=num_runs,ncol=3)
    for (trial in 1:nrow(model_output)){
      model_output[trial,] <- simulate_model(r1,K1,delta,epsilon,omega,prob_disturb, severity_disturb, gamma, distance)
      
      
      
    }
    
    
    
    #total_pop[distance] = mean(model_output[,1])
    #if (mean(model_output[,1])>0.01){ # The 0.01 is a threshold to call the population extinct
    persistence[i] = sum(model_output[,2])
    total_pop[i] = mean(model_output[,1])
    total_var[i] = mean(model_output[,3])
    
    ### option to break loop at speed up code
    best_run = c(best_run,distance_vector[which(persistence==max(persistence,na.rm=T))[1]])
    #print(best_run)
    if (i>10 & all(tail(best_run,10)==tail(best_run,1))){
      #print(length(best_run))
      break}
    
    ###
    #}else{
    #  persistence[distance] = NA
    #  total_pop[distance] = NA
  }
  
  #plot(N1,type='b',ylim=c(0,100))
  #points(N2,ylim=c(0,120),type='b',col='red')
  #}
  
  #points(persistence,col=2,pch=14)
  
  #par(mfrow=c(1,1),oma=c(4,4,0,0))
  #plot(distance_vector,total_pop/max(total_pop), ylab='abundance',pch=16,xlab='',col=1,ylim=c(0,1))
  #abline(v=which(total_pop==max(total_pop)))
  #plot(distance_vector,persistence, ylab='prob near extinct',pch=16,xlab='',col=2)
  #fit4 <- lm(persistence~poly(distance_vector,4,raw=TRUE))
  #lines(distance_vector, predict(fit4, data.frame(x=distance_vector)), col="red")
  #plot(distance_vector,total_var, ylab='variability',xlab='',pch=16,col=2)
  #mtext(text = 'distance between patches',side = 1,outer = T)
  #return(which(persistence==min(persistence)))
  #plot(distance_vector,total_pop)
  #print(max(total_pop))
  #return(which(total_pop==max(total_pop))[1])
  
  #print(min(persistence))
  return(c(distance_vector[which(persistence==max(persistence,na.rm=T))[1]],persistence[which(persistence==max(persistence,na.rm=T))[1]]/num_runs))
  
  #return(c(which(total_pop==max(total_pop))[1], max(total_pop)))
  #my_loess <- loess(near_extinct~distance_vector)
  #return(c(distance_vector[which(predict(my_loess)==min(predict(my_loess)))[1]],max(total_pop[which(predict(my_loess)==min(predict(my_loess)))[1]])))
}

