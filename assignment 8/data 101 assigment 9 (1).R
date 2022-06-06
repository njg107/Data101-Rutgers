library(PermutationTestManual)#Calls specified library.
library(PermutationTestSecond)#Calls specified library.

market <- read.csv("Minimarket.csv")


barplot(tapply(market$TEA, market$COFFEE, mean), col = c("#33FFFF", "green", "yellow", "orange", "red"), xlab = "COOKIES", ylab = "TEA")

p <- Permutation(market, "COFFEE", "TEA", 1000, 0, 1)
p

barplot(tapply(market$COOKIES, market$BREAD, mean), 
        col = c("#33FFFF", "green", "yellow", "orange", "red"), 
        xlab = "BREAD", 
        ylab = "COOKIES")

p <- Permutation(market, "BREAD", "COOKIES", 1000, 1, 0)
p