# Applied Stochastic Processes

# n simulation Galton-Watson Process
# p probability parameter and g generations
# z0 initial population
simulation = function(n,p,g,z0) { #n nombre de simulacions
  # defining a n x (g + 1) matrix
  generation = matrix(data=NA , nrow = g + 1 , ncol = n , byrow=TRUE) 
  # namesim = c(1:n)
  # for (i in namesim) namesim[i]=paste("s",i)
  # namegen = c(0:g)
  # for (i in namegen) namegen[i + 1]=paste("Z",i)
  # dimnames(generation) = list(namegen,namesim)
  # set initial condition
  generation[1,] = z0
  # until generation g
  for (i in (1:g)) { 
    # for the n simulations
    for (j in (1:n)) {
      if (generation[i,j]>0) {
        generation[i+1,j]=rnbinom(1,generation[i,j],p)
      }
      else{
        generation[i+1,j]=0
      }
    }
  }
  generation
}

options(warn=-1)

# testing the model
n=10
p=0.35
g=10
z0=1
matriu_generacions=simulation(n,p,g,z0)
matriu_generacions

# Computing probability of extinguished
generacio9=matriu_generacions[10,]
sum(generacio9==0)/n  # probability of extinguished

simulacio_1 = function(n,p) { #n nombre de simulacions
  generacio = matrix(data=NA , nrow = 11 , ncol = n , byrow=TRUE) # volem fins a la generacio 10, 11 files
  generacio[1,]=1
  for (i in (1:10)) { #arribem fins a la generacio 10
    for (j in (1:n)) {
      if (generacio[i,j]>0) {
        generacio[i+1,j]=rnbinom(1,generacio[i,j],p)
      }
      else{
        generacio[i+1,j]=0
      }
    }
  }
  generacio
}
n=100000
matriu_generacions_1=simulacio_1(n,p); 
generacio9_1=matriu_generacions_1[10,]
sum(generacio9_1==0)/n

# mean and deviation for generation 5 and 10
generacio5_1 = matriu_generacions_1[6,]
generacio10_1 = matriu_generacions_1[11,]
mitjana5 = mean(generacio5_1, na.rm = TRUE); mitjana5
mitjana10 = mean(generacio10_1, na.rm=TRUE); mitjana10
var5 = var(generacio5_1 , na.rm=TRUE); var5
var10 = var(generacio10_1 , na.rm=TRUE); var10

# Histogram
par(mfrow=c(2,1))
hist(generacio5_1 , probability = TRUE , color='cornblue' , main='Generacio n=5, p=0.35')
hist(generacio10_1 , probability = TRUE , color = 'gold' , main='Generacio n=10, p=0.35')

# Changing the probability parameter p
p=1/2
matriu_generacions_nova=simulacio_1(n,p)
generacio9_nova=matriu_generacions_nova[11,]
sum(generacio9_nova==0)/n

# New Histogram
generacio10_nova=matriu_generacions_nova[11,]
par(mfrow=c(1,1))
hist(generacio10_nova , probability = TRUE , color='green' , main='Generacio n=10, p=1/2')

##############################################################################

# Barcelona surname
m = 1229  
z0 = trunc(m/2)  # number of men
n = 10000 # number of simulations
g = 20   # number of generations

p = 0.45  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
par(mfrow=c(2,2))
# plot(survivals, type='b',main="Probability of extinction, p = 0.45", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.45", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , xlab = 'Individuals', main='Generation n=20, p=0.45')

p = 0.55  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
# plot(survivals, type='b',main="Probability of extinction, p = 0.55", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.55", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , xlab = 'Individuals', main='Generation n=20, p=0.55')

p = 0.65  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
# plot(survivals, type='b',main="Probability of extinction, p = 0.65", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.65", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , xlab = 'Individuals', main='Generation n=20, p=0.65')

p = 0.75  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
# plot(survivals, type='b',main="Probability of extinction, p = 0.75", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.75", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , xlab = 'Individuals', main='Generation n=20, p=0.75')

######################################################################

# Cognom Lopez
m = 869461
z0 = trunc(m/2)  # number of men
n = 10000 # number of simulations
g = 20   # number of generations

p = 0.5  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
par(mfrow=c(2,2))
# plot(survivals, type='b',main="Probability of extinction, p = 0.5", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.5", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , main='Generation n=20, p=0.5')

p = 0.6  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
# plot(survivals, type='b',main="Probability of extinction, p = 0.6", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.6", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , main='Generation n=20, p=0.6')

p = 0.7  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
# plot(survivals, type='b',main="Probability of extinction, p = 0.7", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.7", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , main='Generation n=20, p=0.7')

p = 0.8  # probability parameter
matrix_generations = simulation(n,p,g,z0)
mean = c(1:(g+1))
var = c(1:(g+1))
survivals = c(1:(g+1))
for (i in (1:(g+1))) {
  mean[i] = mean(matrix_generations[i,],na.rm=TRUE);
  survivals[i] = sum(matrix_generations[i,]==0)/n;
}
mean
survivals
# plot(survivals, type='b',main="Probability of extinction, p = 0.8", ylab='Prob. of exctinction',xlab='Generation')
# plot(mean, type='b',main="Number of individuals, p = 0.8", ylab='Individuals',xlab='Generation')
generation_g = matrix_generations[(g+1),]
hist(generation_g , probability = TRUE , color='green' , main='Generation n=20, p=0.8')

########################################################################

# Studying the probability of extinction for different processes
# Geometric Distribution
z0 = 1    # initial number
n = 10000 # number of simulations
g = 20    # number of generations

p         = c(0.35,0.45,0.65,0.75)  # probability parameter
p[1]
simulation = function(n,p,g,z0) { #n nombre de simulacions
  # defining a n x (g + 1) matrix
  generation = matrix(data=NA , nrow = g + 1 , ncol = n , byrow=TRUE) 
  # namesim = c(1:n)
  # for (i in namesim) namesim[i]=paste("s",i)
  # namegen = c(0:g)
  # for (i in namegen) namegen[i + 1]=paste("Z",i)
  # dimnames(generation) = list(namegen,namesim)
  # set initial condition
  generation[1,] = z0
  # until generation g
  for (i in (1:g)) { 
    # for the n simulations
    for (j in (1:n)) {
      if (generation[i,j]>0) {
        generation[i+1,j]=rnbinom(1,generation[i,j],p)
      }
      else{
        generation[i+1,j]=0
      }
    }
  }
  generation
}
survivals = matrix(data=NA,nrow=4,ncol=g+1, byrow=TRUE)
for (i in (1:4)) {
  matrix_generations = simulation(n,p[i],g,z0)
  for (j in (1:(g+1))) {
    survivals[i,j] = sum(matrix_generations[j,]==0)/n;
  }
  survivals
}
survivals
par(mfrow=c(1,1))
colors=c("blue", "red", "green" , "black")
plot(survivals[1,],type="b", ylim=c(0,1), xlab='Generation', ylab='Prob. Exctinction', col=colors[1])
for (i in (2:4)) {
  lines(survivals[i,], col=colors[i], type='b')
}
legend("bottomright", legend=c("0.35","0.45","0.65","0.75"), col=colors, lty=1:2, cex=0.8)

# Stationary distributions
teo=c(1,1,1,1)
for (i in (1:4)) {
  if (p[i] >= 1/2) teo[i] = 1
  else teo[i] = (p[i])/(1-p[i])  # geoemetric distribution
}
print(teo)
print(survivals[1,(g+1)])
print(survivals[2,(g+1)])
print(survivals[3,(g+1)])
print(survivals[4,(g+1)])

#############################################################################

# Change the simulation to Poisson Process

simulation = function(n,p,g,z0) { #n nombre de simulacions
  # defining a n x (g + 1) matrix
  generation = matrix(data=NA , nrow = g + 1 , ncol = n , byrow=TRUE) 
  # namesim = c(1:n)
  # for (i in namesim) namesim[i]=paste("s",i)
  # namegen = c(0:g)
  # for (i in namegen) namegen[i + 1]=paste("Z",i)
  # dimnames(generation) = list(namegen,namesim)
  # set initial condition
  generation[1,] = z0
  # until generation g
  for (i in (1:g)) { 
    # for the n simulations
    for (j in (1:n)) {
      if (generation[i,j]>0) {
        generation[i+1,j]=rpois(1,generation[i,j]*p)
      }
      else{
        generation[i+1,j]=0
      }
    }
  }
  generation
}

# Studying the probability of extinction for different processes
# Poisson Distribution
z0 = 1    # initial number
n = 10000 # number of simulations
g = 20    # number of generations

p         = c(0.8,0.9,1.0,1.1)  # probability parameter
p[1]
survivals = matrix(data=NA,nrow=4,ncol=g+1, byrow=TRUE)
for (i in (1:4)) {
  matrix_generations = simulation(n,p[i],g,z0)
  for (j in (1:(g+1))) {
    survivals[i,j] = sum(matrix_generations[j,]==0)/n;
  }
  survivals
}
survivals

par(mfrow=c(1,1))
colors=c("blue", "red", "green" , "black")
plot(survivals[1,],type="b", ylim=c(0,1), xlab='Generation', ylab='Prob. Exctinction', col=colors[1])
for (i in (2:4)) {
  lines(survivals[i,], col=colors[i], type='b')
}
legend("bottomright", legend=c("0.8","0.9","1.0","1.1"), col=colors, lty=1:2, cex=0.8)

# Stationary distributions
teo=c(1.0,
      1.0,
      1.0,
      0.8238658563681903)
print(teo)
print(survivals[1,(g+1)])
print(survivals[2,(g+1)])
print(survivals[3,(g+1)])
print(survivals[4,(g+1)])

##########################################################################

# Change the simulation to Binomial Process

simulation = function(n,N,p,g,z0) { #n nombre de simulacions and N the other parameter for the binomial
  # defining a n x (g + 1) matrix
  generation = matrix(data=NA , nrow = g + 1 , ncol = n , byrow=TRUE) 
  # namesim = c(1:n)
  # for (i in namesim) namesim[i]=paste("s",i)
  # namegen = c(0:g)
  # for (i in namegen) namegen[i + 1]=paste("Z",i)
  # dimnames(generation) = list(namegen,namesim)
  # set initial condition
  generation[1,] = z0
  # until generation g
  for (i in (1:g)) { 
    # for the n simulations
    for (j in (1:n)) {
      if (generation[i,j]>0) {
        generation[i+1,j]=rbinom(1,generation[i,j]*N,p)
      }
      else{
        generation[i+1,j]=0
      }
    }
  }
  generation
}

# Studying the probability of extinction for different processes
# Binomial Distribution
z0 = 1    # initial number
n = 10000 # number of simulations
g = 20    # number of generations
N = 3

p         = c(0.6,0.5,0.4,0.3)  # probability parameter
p[1]
survivals = matrix(data=NA,nrow=4,ncol=g+1, byrow=TRUE)
for (i in (1:4)) {
  matrix_generations = simulation(n,N,p[i],g,z0)
  for (j in (1:(g+1))) {
    survivals[i,j] = sum(matrix_generations[j,]==0)/n;
  }
  survivals
}
survivals

par(mfrow=c(1,1))
colors=c("blue", "red", "green" , "black")
plot(survivals[1,],type="b", ylim=c(0,1), xlab='Generation', ylab='Prob. Exctinction', col=colors[1], main = 'N = 3')
for (i in (2:4)) {
  lines(survivals[i,], col=colors[i], type='b')
}
legend("bottomright", legend=c("0.6","0.5","0.4","0.3"), col=colors, lty=1:2, cex=0.8)

# Stationary distributions
print(survivals[1,(g+1)])
print(survivals[2,(g+1)])
print(survivals[3,(g+1)])
print(survivals[4,(g+1)])
teo=c(0.0957118462605635,0.236067977499790,0.557189138830738,1.00000000000000)
print(teo)
