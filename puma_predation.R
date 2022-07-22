# Importar e visualizar a minha planilha .csv como um objeto no R 
pecj<-read.csv("puma_predation.csv",header=T,sep=",",dec=".")
summary(pecj)

# Renomear variavel resposta (y = 1 ou 0)
predation<-pecj$predation

# Teste nao-parametrico de correlacao para testar a existencia de associacao 
cor.test(pecj$livestock,pecj$dogs) # numero de criacoes com o numero de caes
cor.test(pecj$elevation,pecj$treecover) # elevacao com porcentagem de cobertura arborea
cor.test(pecj$livestock,pecj$area) # numero de criacoes com o tamanho da area
cor.test(pecj$livestock,pecj$elevation) # numero de criaacoes com a elevacao
cor.test(pecj$livestock,pecj$centre) # numero de criacoes com a distancia do centro urbano
cor.test(pecj$elevation,pecj$centre) # elevacao com a distancia do centro urbano
cor.test(pecj$area,pecj$elevation) # tamanho da area com a elevacao
cor.test(pecj$area,pecj$centre) # tamanho da area com a distancia do centro urbano

# Plotar as variaveis correlacionadas em um grafico de dispersao 	
plot(pecj$livestock,pecj$dogs) # numero de criacoes com o numero de caes
plot(pecj$elevation,pecj$treecover) # elevacao com porcentagem de cobertura arborea

# Normalizando as variaveis continuas (explanatorias) nao-correlacionadas entre si
live.mean<-mean(pecj$livestock)
live.sd<-sd(pecj$livestock)
live.z<-(pecj$livestock-live.mean)/live.sd # numero de criacoes (n)

area.mean<-mean(pecj$area)
area.sd<-sd(pecj$area)
area.z<-(pecj$area-area.mean)/area.sd # tamanho da area (ha)

ele.mean<- mean(pecj$elevation)
ele.sd<-sd(pecj$elevation)
ele.z<-(pecj$elevation-ele.mean)/ele.sd	# elevacao (m)

centre.mean<-mean(pecj$centre)
centre.sd<-sd(pecj$centre)
centre.z<-(pecj$centre-centre.mean)/centre.sd # distancia do centro urbano (m) "Campos do Jordao"

# Ajustar os modelos lineares generalizados (glm): binomial
glm0<-glm(predation~1, data=pecj, family=binomial) # predacao constante/independente/aleatoria ("constant model")
glm1<-glm(predation~live.z+area.z, data=pecj, family=binomial) # predacao dependente do numero de criacoes e do tamanho da area
glm2<-glm(predation~ele.z+centre.z, data=pecj, family=binomial) # predacao dependente da elevacao e da distancia do centro
glm3<-glm(predation~live.z+area.z+ele.z, data=pecj, family=binomial) # predacao dependente do numero de criacoes, do tamanho da area e da elevacao 
glm4<-glm(predation~live.z+area.z+centre.z, data=pecj, family=binomial) # predacao dependente do numero de criacoµes, do tamanho da area e da distancia do centro urbano
glm5<-glm(predation~live.z+ele.z+centre.z, data=pecj, family=binomial) # predacao dependente do numero de criacoes, da elevacao e da distancia do centro urbano
glm6<-glm(predation~area.z+ele.z+centre.z, data=pecj, family=binomial) # predacao dependente do tamanho da area, da elevacaa e da distancia do centro urbano
glm7<-glm(predation~live.z+area.z+ele.z+centre.z, data=pecj, family=binomial) # predacao dependente do numero de criacoes, do tamanho da area, da elevacaoo e da distancia do centro urbano (modelo super-parametrizado ou "full model")

# Abrir o pacote
library(AICcmodavg)

# Rankear os modelos pelo AIC
Cand.mods1 <- list(glm0, glm1, glm2, glm3, glm4, glm5, glm6, glm7)
Modnames1 <- c("glm0.constant","glm1.live+area","glm2.elev+centre",
               "glm3.live+area+elev", "glm4.live+area+centre", 
               "glm5.live+elev+centre","glm6.area+elev+centre",
               "glm7.global")
aictab(cand.set = Cand.mods1, modnames = Modnames1, second.ord = TRUE)

#Extrair as estimativas do melhor modelo
coef(glm2) # coefficients from objects
confint (glm2) # confidence intervals (CI) for parameters
anova(glm2) # compute analysis of variance for fitted model
plogis(0.05) # inverse link function to intercept coefficient
plogis(-0.62) # 2.5% confit (CI)
plogis(0.75) # 97.5% confint (CI)
plogis(-0.13) # mean
plogis(-1.65) # min
plogis(1.73) # max

png(file = "glm_predation.png", width = 800, height = 700)

par(mar = c(7, 10, 7, 10))
# Plotar a probabilidade de predação do modelo ajustado
plot(ele.z + centre.z, fitted(glm2), 
     xlab= "Elevation and distance from the urban center", 
     ylab="Probability of predation",
     pch = 16, cex = 1.5, col="red",
     cex.lab=2.2, cex.axis=1.7) 
legend("bottomright", legend = c("Properties"), cex = 1.5, lwd = 3, lty=0, pch=16, col="red")
curve(predict(glm2, type = c("response"), add=T))
fitted.values(glm2)
residuals(glm2)
predict(glm2)

dev.off()

library(magick)
library(magrittr) 

# Call back the plot
plot <- image_read("glm_predation.png")
plot2<-image_annotate(plot, "Livestock Predation by Puma in the Highlands
of a Southeastern Brazilian Atlantic Forest", 
                      color = "blue", size = 25,
                      location = "10+50", gravity = "north")
plot3<-image_annotate(plot2, "Data: Palmeira et al 2015 (doi.org/10.1007/s00267-015-0562-5) | Image credit: @PhyloPic | Visualization by @fblpalmeira", 
                      color = "gray", size = 15, 
                      location = "10+50", gravity = "southeast")
# And bring in a logo
logo_raw <- image_read("http://www.phylopic.org/assets/images/submissions/87c44856-307d-4d1a-84fd-ec54f8591f1a.512.png") 
out<-image_composite(plot3,image_scale(logo_raw,"x70"), offset = "+160+110")
image_browse(out)

# And overwrite the plot without a logo
image_write(out, "puma_predation.png")
