
a = 2

a
str( a )
class( a )


b <- 3

b

a + b

a b

a + d

a + 'b'


(a + b) == (2 + 3)



rand_100_norm <- rnorm(1000, mean = 5, sd = 10)

plot(rand_100_norm, type = 'l', lty = 2, col = 'blue', main = 'Ejemplo')

hist(rand_100_norm, col = 'green')

plot(density(rand_100_norm) )

hist(rand_100_norm, col = 'green', probability = TRUE)
lines(density(rand_100_norm), col = 'red' )


library(raster)

rast1 <- raster( x = matrix(rnorm(100), ncol = 10, nrow = 10) )
plot(rast1)


rast2 <- rast1
rast2[45] <- 100
plot(rast2)

rast2[ which(rast2[] == 100) ] <- 10
plot(rast2)


for (i in 1:10){
  print(i)
  rast1 <- raster( x = matrix(rnorm(100), ncol = 10, nrow = 10) )
  plot(rast1, main = i)
  Sys.sleep(2)
}



# Esto es un comentario
Esto es un error
"Esto no es un error"

paste( "Esto no es un error", 1000, "ya es break")
