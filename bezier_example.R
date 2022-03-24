library(dplyr)
library(ggplot2)
library(bezier)
library(tibble)

theme_set(theme_minimal() )



get_bezier_points <- function(points) {
  
  n <- length(points) / 3
  
  
  t <- seq(0, n, length=100)
  
  bz <- bezier(t=t, p=points, deg=2)
  
  data.frame(x=bz[,1], y=bz[,2])
  
  
}





# Oferta
library(polynom)
oferta_1 <- poly.calc(x=c(3, 10, 15), 
                      y=c(0, 20, 35)) %>% 
  as.function()

oferta_2 <- poly.calc(x=c(13, 20, 25), 
                      y=c(5, 25, 40)) %>% 
  as.function()


# Demanda inelastica
puntos_demanda_inelastica <- c(7.5, 40, 
                               8.35, 30, 
                               10,20,
                               11, 10,
                               13, 5)

ps <- matrix(puntos_demanda_inelastica, ncol=2, byrow = TRUE)
demanda_inelastica_bz <- get_bezier_points(ps) %>% 
  filter(x<15)


ggplot() + 
  geom_function(fun=oferta_1, xlim=c(5, 15)) +
  geom_function(fun=oferta_2, xlim=c(12.5, 23.25)) +
  geom_line(aes(x, y), data=demanda_inelastica_bz) +
  # Eje Y
  geom_segment(aes(x=0, y=0, xend=0, yend=40), 
               arrow=arrow(type="closed", length=unit(.25, "cm")),
               alpha=.6) + 
  # Eje X
  geom_segment(aes(x=0, y=0, xend=25, yend=0), 
               arrow=arrow(type="closed", length=unit(.25, "cm")),
               alpha=.6) +
  expand_limits(x=c(0, 25), y=c(0, 40)) +
  geom_point(aes(x,y), 
             data=tibble(x=c(10, 13),
                         y=c(20, 5)))


oferta_3 <- poly.calc(x=c(13, 17, 22.5), 
                      y=c(5, 15, 30)) %>% 
  as.function()


# demanda Elastica
puntos_demanda_elastica <- c(3, 40, 
                             5, 28, 
                             10,20,
                             12.5, 17,
                             17, 15)

demanda_elastica_bz <- matrix(puntos_demanda_elastica, 
                              ncol=2, byrow = TRUE) %>% 
  get_bezier_points()

ggplot() + 
  geom_function(fun=oferta_1, xlim=c(5, 15), size=1) +
  geom_function(fun=oferta_3, xlim=c(12.5, 23.25), size=1) +
  scale_x_continuous(breaks=c(10, 17),
                     expand = expand_scale(add=0.5),
                     limits=c(0, 25)) +
  scale_y_continuous(breaks=c(20, 15),
                     expand = expand_scale(add=1)) +
  # Eje Y
  geom_segment(aes(x=0, y=0, xend=0, yend=40), 
               arrow=arrow(type="closed", length=unit(.25, "cm")),
               alpha=.6) + 
  # Eje X
  geom_segment(aes(x=0, y=0, xend=25, yend=0), 
               arrow=arrow(type="closed", length=unit(.25, "cm")),
               alpha=.6) +
  geom_line(aes(x, y), data=demanda_elastica_bz, size=1) +
  expand_limits(x=c(0, 25), y=c(0, 40)) +
  geom_point(aes(x,y), 
             data=tibble(x=c(10, 17),
                         y=c(20, 15))) +
  labs(x="Q", y="P")



