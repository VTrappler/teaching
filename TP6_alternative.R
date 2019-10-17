# TP6: Alternative si le serveur shiny est inaccessible
# Éxecutez la fonction suivante (ça ne sert à rien de regarder dedans)
create.CI <- function(NCI, Nsamples, mu=0, sigma=1, niveau=0.05){
  samples <- rnorm(NCI * Nsamples, mu, sigma)
  samples <- matrix(data = samples, nrow=Nsamples, ncol=NCI)
  mean.vec <- colMeans(samples)
  sd.vec <- sqrt(apply(samples, 2, FUN=var))
  quant <- qt(1 - (niveau/2), Nsamples - 1)
  CI <- cbind(mean.vec - quant*sd.vec / sqrt(Nsamples), mean.vec + quant*sd.vec / sqrt(Nsamples))
  plot(c(mu, mu), c(0, NCI), col='red', type='l', xlab='', ylab='', ylim=c(0.5, NCI+0.5), xlim=c(mu - 4*sigma/sqrt(Nsamples), mu+4*sigma/sqrt(Nsamples)) )
  count = 0
  for (i in 1:NCI){
    lines(CI[i, ], c(i, i)) 
    if (mu >= CI[i, 1] & mu <= CI[i, 2]){
      count = count + 1
    }
  }
  percent = 100*(1- count / NCI)
  str = sprintf("Il y a %s %% d'IC qui ne contiennent par la vraie valeur mu=%s", percent, mu)
  title(str)
  abline(v=mu, col='red')
  
  }


NombreIntervallesdeConfiance <- 1000
NombreEchantillonsparIC <- 1000
muVrai <- 0
varianceVraie <- 10
niveaudeconfiance <- 20
create.CI(NombreIntervallesdeConfiance, NombreEchantillonsparIC, muVrai, varianceVraie, niveaudeconfiance/100)
