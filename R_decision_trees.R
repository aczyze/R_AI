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
library("rpart.plot")
prp(drzewo, type=3, extra=1)

