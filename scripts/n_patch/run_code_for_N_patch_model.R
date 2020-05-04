


  
# This line specifies which reserve combinations someone wants to run. It is best to leave out reserves on the ends as they would have high mortality sending recruits outside the reserve
configs_to_try = t(combn((extra_patches+3):(extra_patches+num_patches-2), num_reserves))
result=matrix(0,nrow=nrow(configs_to_try),ncol=(num_reserves+1))

for (index1 in 1:nrow(configs_to_try)){
  x1=rep(1,total_patches);x1[as.numeric(configs_to_try[index1,])]=0
   result[index1,]=c(which(x1==0),evalFunc(x1))
 
 #result=matrix(0,nrow=5,ncol=(num_reserves+1))
# for (index1 in 1:5){
#   x1=rep(1,total_patches);x1[c(15,15+index1)]=0
#   result[index1,]=c(which(x1==0),evalFunc(x1))
#   print(result)
 }
#}
  ranked_configs = result[order(result[,num_reserves+1]),1:(num_reserves+1)]
  num_near_top = which(ranked_configs[,(num_reserves+1)] <= ranked_configs[1,(num_reserves+1)]-0.001*ranked_configs[1,(num_reserves+1)])
  
  
  # if (length(num_near_top)>20){
  #   optimal_reserves=rep(NA,num_reserves)
  #   #optimal_spacing[value] = NA
  #   #print(optimal_spacing[value])
  # }else{

    
    optimal_reserves= ranked_configs[1,1:num_reserves]
#}
   # optimal_spacing[value] = diff(optimal_reserves)#mean(mean(apply(optimal_reserves,1,FUN=diff)))
   # mean_objective_value[value]=-ranked_configs[1,1+num_reserves]#mean(-ranked_configs[num_near_top,(num_reserves+1)])/(max_time*num_trials)
    
    #optimal_spacing[value] = diff(optimal_reserves[1,])
    #print(optimal_spacing[value])
    #print(length(num_near_top))
  
    #test=rep(1,num_patches)
    #test[optimal_reserves[value,]]=0
    #print(evalFunc(test))
  
