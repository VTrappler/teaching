# TP6 Exercice 1 Alternatif si le serveur shiny est inaccessible
# Éxecutez la fonction suivante (ça ne sert à rien de regarder dedans)
create.CI <- function(NCI, Nsamples, mu=0, variance=1, niveau=0.05){
  samples <- rnorm(NCI * Nsamples, mu, sqrt(variance))
  samples <- matrix(data = samples, nrow=Nsamples, ncol=NCI)
  mean.vec <- colMeans(samples)
  sd.vec <- sqrt(apply(samples, 2, FUN=var))
  quant <- qt(1 - (niveau/2), Nsamples - 1)
  CI <- cbind(mean.vec - quant*sd.vec / sqrt(Nsamples), mean.vec + quant*sd.vec / sqrt(Nsamples))
  plot(c(mu, mu), c(0, NCI), col='red', type='l', xlab='', ylab='', ylim=c(0.5, NCI+0.5), xlim=c(mu - 4*sigma/sqrt(Nsamples), mu+4*sigma/sqrt(Nsamples)) )
  count = 0
  for (i in 1:NCI){
    lines(CI[i, ], c(i, i), col='blue') 
    if (mu >= CI[i, 1] & mu <= CI[i, 2]){
      count = count + 1
    }
  }
  percent = round(100*(1- count / NCI))
  str = sprintf("Il y a %s %% d'IC qui ne contiennent par la vraie valeur mu=%s", percent, mu)
  title(str)
  abline(v=mu, col='red')
}

# Changez les paramètres ici
nombre.Echantillons <- 100
taille.Echantillons <- 50
mu.theorique <- 0
variance.theorique <- 1
niveau.confiance <- 0.95
# Puis appelez cette fonction
create.CI(nombre.Echantillons, taille.Echantillons, mu.theorique, variance.theorique, 1-niveau.confiance)
