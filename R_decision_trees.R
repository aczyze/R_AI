library(rpart)
attach(iris)

iris[c(1,60,120),]
tk <- table (iris["Species"])
print(tk)

drzewo <- rpart(Species~., data=iris,
                control=rpart.control(cp=0, 
                    minsplit=2,minbucket=1))
#Species~ oznacza, ze liscie beda odpowiadaly
#etykietom(klasom) opisanym przez wartosci atrybutu
#Species a w procesie tworzenia drzewa beda brane pod uwage
#wszystkie pozostale atrybuty(co oznaczono kropka) ze zbioru
#danych trenujacych iris
print(drzewo)

#dodatkowy pakiet zeby dostac postac graficzna drzewa
#install.packages("RGtk2", depen=T)
#roziazanie problemu ktory wyskakiwal:
#install.packages("rattle")
#install.packages("RGtk2")
library("rpart.plot")
prp(drzewo, type=3, extra=1)

#powstalo drzewo pelne
#info o parametrze cp okreslajacym wielkosc drzewa
#w zaleznosci od bledu klasyfikacji
printcp(drzewo)

#!!wartosc cp decyduje o wielkosci przyjecia drzewa
#z kodu wygenerowanego przez "printcp(drzewo)" mozemy
#odczytac, ze najmniejszy wzgledny blad oceny krzyzowej
#(xerror) wynosi 0,09 przy odchyleniu standardowym 
#(xstd) 0,029086
#zatem jako najlepsze przyjmiemy drzewo ktorego blad
#klasyfikacji jest ponizej 0,09+0,029086 (regula + 1 SE)
#mozna to pokazac graficznie:
plotcp(drzewo)
#optymalne jest zwykle najmniejsze drzewo ktorego blad
#jest ponizej bledu z reguly 1SE

drzewo.p <- prune(drzewo, cp=drzewo$cptable[3])
prp(drzewo.p, type=3, extra=1)

#testujemy teraz wydzielajac zbior testowy:
  ##ustawienie pocz¹tkowe generatora liczb pseudolosowych
set.seed(123)
  ##liczba danych
l.d <- nrow(iris)
  ##wygenerowanie losowo numerów przyk³adów testowych:
test <- sample(1:l.d, round(l.d/3),replace = FALSE)
  ##zbior danych trenujacych
iris.tr <- iris[-test,]
  ##zbior danych testowych
iris.test <- iris[test,]
drzewo <- rpart(Species~., data=iris.tr, method="class",
                control=rpart.control(cp=0, minsplit = 3,
                                      minbucket = 1))
plotcp(drzewo)
printcp(drzewo)
drzewo.p <- prune(drzewo, cp=drzewo$cptable[3])
  ##blad klasyfikacji
y.pred <- predict(drzewo.p, newdata = iris.test, 
                  type="class")
blad <- 1-sum(y.pred==iris.test$Species)/nrow(iris.test)
print(blad)
  ##jezeli dane sa wylacznie liczbowe, zeby uzyskac
  ##drzewo klasyfikacyjne trzeba uzyc argumentu
  ##method="class" przy tworzeniu drzewa
  ##oraz type="class" przy predykcji




