# initial model
lvmod.1 <- ' 
# Latent variable definition
Perform=~ stems + infls + clonediam
+ leafht + leafwdth'

# fit model (note we get a warning, but it is not a fatal error)
lvmod.1.fit <- sem(lvmod.1, data=perf.dat)
summary(lvmod.1.fit, rsq=T, standardized=T)

# request modification indices (several choices here)
modindices(lvmod.1.fit)  #this gives us everything
mi <- modindices(lvmod.1.fit) #create index object
print(mi[mi$op == "~",])   #request only ~ links
print(mi[mi$op == "~~",])  #request only ~~ links
print(mi1[([mi1$mi > 3.0,] & [!(mi1$mi=="<NA>"),])]) # request only large ones

# modified CFA model with error covariance added
lvmod.2 <- ' # Latent variable definition
Perform=~ stems + infls + clonediam
+ leafht + leafwdth

# Error Covariances
leafht ~~ leafwdth'

# fit model
lvmod.2.fit <- sem(lvmod.2, data=perf.dat)
summary(lvmod.2.fit, rsq=T, standardized=T)

##### Putting performance into context in the full model
# specify model
lvmod.3 <- ' # Latent variable definition
Perform=~ stems + infls + clonediam
+ leafht + leafwdth

# Error Covariances
leafht ~~ leafwdth

# Regressions
Perform ~ geneticdist
leafht ~ latitude
leafwdth ~ latitude'

# fit model
lvmod.3.fit <- sem(lvmod.3, data=perf.dat)
summary(lvmod.3.fit, rsq=T, standardized=T)


#############################
growth<-'growth=~Shoot.number+Shoot.diameter+Greatest.height+Accumulative.height+
Rhizome.length+Rhizome.diameter+Stem.DM
Shoot.number ~~ Stem.DM
Shoot.number ~~  Rhizome.length
Shoot.number ~~ Greatest.height
Rhizome.length ~~          Stem.DM
Greatest.height ~~   Rhizome.length
Greatest.height ~~ Stem.DM
Shoot.number ~~ Accumulative.height
Shoot.diameter ~~ Accumulative.height 
Greatest.height ~~ Accumulative.height
Accumulative.height ~~ Stem.DM
growth~Lat+bio36+bio37+bio38+bio39+bio40
'
Growth.ci<-cfa(growth, data=both2,meanstructure=TRUE,fixed.x=F)
summary(Growth.ci,fit.measures=TRUE,rsq=TRUE,standardized=TRUE)
mi<-modindices(Growth.ci)
print(mi[mi$mi>3.84,])

growth<-'growth=~Shoot.number+Shoot.diameter+Greatest.height+
Rhizome.length+Rhizome.diameter+Stem.DM+Stem.allocation
Shoot.number ~~ Stem.DM
Shoot.number ~~  Rhizome.length
Shoot.number ~~ Greatest.height
Rhizome.length ~~          Stem.DM
Greatest.height ~~   Rhizome.length
Greatest.height ~~ Stem.DM
Greatest.height ~~ Stem.allocation
Rhizome.diameter ~~ Stem.allocation
growth~Lat+Long+bio36+0*bio37+bio38+bio39+bio40
bio36~Lat+Long
bio38~Lat+Long
bio39~Lat+Long
bio40~Long+Lat
bio36~~bio39
bio38~~bio39
Lat~~Long
Lat~~bio37
Long~~bio37
bio38~~bio37
'
Growth.ci<-cfa(growth, data=both2,meanstructure=TRUE,fixed.x=F)
summary(Growth.ci,fit.measures=TRUE,rsq=TRUE,standardized=TRUE)
mi<-modindices(Growth.ci)
print(mi[mi$mi>3.84,])

library(semPlot)
pdf("SEM.pdf", width=18.5, height=10)
par(mfrow=c(1,2))
semPaths(Growth.ci,"std")
dev.off()