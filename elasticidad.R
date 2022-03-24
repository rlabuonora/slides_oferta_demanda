library(ggrepel)

dem <- tibble(
  q=c(160, 200, 240),
  p=c(110, 100, 90),
  lab=c("B", "", "A")
)


dem_p <- poly.calc(dem$q, dem$p) %>% 
  as.function

ggplot() + 
  geom_function(fun=dem_p, xlim=c(120, 280), size=1) +
  geom_point(data=dem, aes(x=q, y=p)) +
  # Etiquetas para los puntos
  geom_text(aes(x=q+10, y=p+10, label=lab), data=dem) +
  scale_x_continuous(breaks=seq(0, 360, by=40)) + 
  scale_y_continuous(breaks=seq(0, 200, by=20)) + 
  expand_limits(y=c(0, 200),
                x=c(0, 360)) + 
  # Eje Y
  geom_segment(aes(x=0, y=0, xend=0, yend=200)) + 
  # Eje X
  geom_segment(aes(x=0, y=0, xend=360, yend=0)) + 
  # Highlight points
  geom_segment(aes(x=0, y=90, xend=240, yend=90), alpha=.6) +
  geom_segment(aes(x=0, y=110, xend=160, yend=110), alpha=.6) + 
  geom_segment(aes(x=240, xend=240, y=90, yend=0), alpha=.6) +
  geom_segment(aes(x=160, xend=160, y=110, yend=0), alpha=.6) +
  labs(x="Q", y="P")

ggplot() + 
  geom_segment(aes(x=20, y=100, xend=300, yend=100), size=1) +
  geom_segment(aes(x=200, y=0, xend=200, yend=200), size=1) +
  scale_x_continuous(breaks=seq(0, 360, by=40)) + 
  scale_y_continuous(breaks=seq(0, 200, by=20)) +
  annotate("text", x=240, y=180, label="Demanda\ninelástica") + 
  annotate("text", x=80, y=80, label="Demanda elástica") + 
  
  coord_fixed() +
  expand_limits(y=c(0, 200),
                x=c(0, 360)) + 
  # Eje Y
  geom_segment(aes(x=0, y=0, xend=0, yend=200)) + 
  # Eje X
  geom_segment(aes(x=0, y=0, xend=360, yend=0)) + 
  labs(x="Q", y="P", title="Casos Extremos")


  
