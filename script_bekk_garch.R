#**************************************************************#
######################### 3-BEKK-GARCH ##########################
#**************************************************************#
bekk.spec = garchSpec(model = list(alpha = 0.2, beta = 0.7))
bekk.A0=matrix(c(0.1,0,0,-0.1,0.2,0,0.3,-0.1,0.4),ncol=3,byrow=T)
bekk.A1=matrix(c(0.7,0,0.1,0,0.2,-0.1,0.2,-0.1,0.4),ncol=3,byrow=T)
bekk.B1=matrix(c(0.2,0,-0.5,0,0,0.2,0,-0.1,0.1),ncol=3,byrow=T)
bekk.S0=diag(3)
bekk.epsilon1=garchSim(bekk.spec, n = 1000)
bekk.epsilon2=garchSim(bekk.spec, n = 1000)
bekk.epsilon3=garchSim(bekk.spec, n = 1000)
bekk.epsilon<-cbind(bekk.epsilon1,bekk.epsilon2,bekk.epsilon3)

bekk.S<-array(0,dim=c(1000,3,3))
bekk.S[1,,]=diag(3)

for(t in 2:1000){  
  bekk.S[t,,]=bekk.A0%*%t(bekk.A0) + bekk.A1 %*%  t(bekk.epsilon[t,]) %*% bekk.epsilon[t,] %*% t(bekk.A1) + bekk.B1%*%bekk.S[t-1,,]
}
MTSplot(bekk.S)

###### 3.1-Simulacao utilizando o pacote mgarch ##### 
bekk.sim <- mvBEKK.sim(series.count = 3, T = 2500)
names(bekk.sim)
# > names(bekk.sim)
# [1] "length"            "series.count"      "order"            
# [4] "params"            "true.params"       "eigenvalues"      
# [7] "uncond.cov.matrix" "white.noise"       "eps"              
# [10] "cor"               "sd" 
bekk.sim$length
bekk.sim$series.count
bekk.sim$order
bekk.sim$params
bekk.sim$true.params
bekk.sim$eigenvalues
bekk.sim$uncond.cov.matrix
matrix.bekk.sim<-cbind(bekk.sim$eps[[1]],bekk.sim$eps[[2]],bekk.sim$eps[[3]])
plot.ts(matrix.bekk.sim)

##### 3.1.a-Estimacao do BEKK via mgarch #####
fit.bekk.mgarch<-mvBEKK.est(matrix.bekk.sim)
##### 3.2-Estimacao do BEKK via MTS #####
system.time(fit.bekk.mts<-BEKK11(matrix.bekk.sim))
names(fit.bekk.mts)
# [1] "estimates"  "HessianMtx" "Sigma.t"

fit.bekk.mts$estimates