#-----ROZDZIAL 2 SZTUCZNE SIECI NEURONOWE

#--PRZYKLAD APROKSYMACJI FUNKCJI JEDNEJ ZMIENNEJ

#install.packages("AMORE", dependencies = TRUE)

library(AMORE)

x<- seq(from=0, to=7, by=0.1)
set.seed(5)
z<- 10-12*x+4*x^2-0.3*x^3+runif(length(x),-2,2)
plot(x, z, pch=19, col="blue")

par(mar=c(5,5,1.9,1.9)) 

siec <- newff(n.neurons=c(1,2,1),learning.rate.global = 0.001,
        momentum.global=0.5,error.criterium="LMS",
        hidden.layer="sigmoid", output.layer="purelin",
        method="ADAPTgdwm")

set.seed(5)
#seed daje nam powtarzalne losowe wyniki!!!
wynik <- train (siec, x, z, error.criterium = "LMS", 
                report=TRUE, show.step = 10, n.shows=200)

plot(wynik$Merror, type='l',xlab="Iteracja(x10)", 
     ylab = "B³¹d", col="darkred")


#generujemy dane testowe zeby zobaczyc czy siec 
#dobrze aproksymuje funkcje
xt<- seq(from=0, to=7, by=0.18)
set.seed(8)
zt<- 10-12*xt+4*xt^2-0.3*xt^3+runif(length(xt),-2,2)
#i obliczamy wyjscia sieci dla testowych wartosci xt
y<- sim(wynik$net,xt)

#ocenimy model za pomoc¹  RMSE
#najpierw zapiszemy funkcjê
#sa wbudowane np. err.lmls a my piszemy swoja
err.rmse <- function(zad, wy)
  {
    sqrt(mean((zad-wy)^2))
  }

err.rmse(zt,y)

#graficznie
plot(xt,zt,col="blue", pch=3, xlab="x_t", ylab="z_t")
lines(xt,y,col="red")


#--PRZYKLAD ZADANIA KLASYFIKACJI

data("iris")
iris
table(iris$Species)

#dzielimy dane na trenujace i test. wg. zasady 2/3(ze 150)
l.danych <- nrow(iris)
set.seed(8)
idxTren <- sample(1:l.danych, 100)
idxTest <- setdiff(1:l.danych, idxTren)

#dane tekstowe trzeba dla sieci przeksztalcic na liczby
target <- function(b)
  {
    n <- length(b)
    wartosci <- levels(b)
    l<-length(wartosci)
    T<-matrix(0,nrow = n, ncol=l)
    for(i in 1:l)
      T[, i] <- (b==wartosci[i])
    colnames(T) <- wartosci
    return(T)
}

wZadane <- target(iris$Species)
wZadane

#robimy siec
set.seed(2)
siecIris <- newff(n.neurons = c(4,3,3),
              learning.rate.global = .02,
              momentum.global = 0.5,
              hidden.layer = "sigmoid",
              output.layer = "purelin",
              method="ADAPTgdwm",
              error.criterium = "LMS")

#trenujemy na zestawie treningowym bez 5 kol. nazwy
#idxTren to po prostu numery wierszy
#porownujemy wiersze bez ostatniej kol. do pe³nych wierszy
wynikIris <- train(siecIris, iris[idxTren,-5], wZadane[idxTren,],
               error.criterium = "LMS", report = TRUE,
               show.step=10,
               n.shows = 800)

plot(wynikIris$Merror,type='l', xlab="Iteracja (x10)",
     ylab="B³¹d", col="darkred")

yi<-sim(wynikIris$net, iris[idxTest,-5])
yi

#w wyniku dostajemy szacowan¹ wartoœæ wyjœcia, nie koniec!
test.klasyf <- function(zad,wy)
  {
    zadane <- max.col(zad)
    rozpoznane <- max.col(wy)
    print(table(zadane,rozpoznane))
  }

#dostajemy macierz pomy³ek
wynikIris <- test.klasyf(wZadane[idxTest,], yi)

cat("Dok³adnoœæ klasyfikacji:",
    sum(diag(wynikIris))/sum(wynikIris)*100,"%")


#--PRZYKLAD ZADANIA KLASYFIKACJI Z ZASTOSOWANIEM
#--OCENY KRZYZOWEJ Z KROKIEM 10

library(AMORE)
data("iris")
l.danych <- nrow(iris)
wZadane <- target(iris$Species)

siecIrisK <- newff(n.neurons = c(4,3,3),
                   learning.rate.global = .02,
                   momentum.global=0.5,
                   hidden.layer = "sigmoid",
                   output.layer = "purelin",
                   method="ADAPTgdwm",
                   error.criterium="LMS")

k<- 10
set.seed(15)
idx<- sample(1:l.danych,l.danych,replace=FALSE)
krok <- c(round(seq(from=0,to=l.danych-1,by=l.danych/k),0),
          l.danych)
blad<- c()
for(i in 1:k)
{
  idxTestK <- idx[(krok[i]+1):krok[i+1]]
  wynikIrisK <- train(siecIrisK, iris[-idxTest,-5], 
                      wZadane[-idxTest,],
                      error.criterium = "LMS",
                      report=FALSE,
                      show.step = 10, n.shows = 800)
  yik <- sim(wynikIrisK$net, iris[idxTestK,-5])
  wynikIrisK <- test.klasyf(wZadane[idxTestK,],yik)
  blad[i] <- 1-sum(diag(wynikIrisK))/sum(wynikIrisK)
}

cat("Dok³adnoœæ klasyfikacji:", (1-mean(blad))*100,"%")



