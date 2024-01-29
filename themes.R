#theme maximal

diamonds %>% 
  ggplot(aes(cut,carat, colour=price))+geom_jitter()+ggtitle("Diamonds.")


D<-diamonds %>% 
  ggplot(aes(cut,carat, colour=price))+geom_jitter()+ggtitle("Diamonds.")

theme_maximal<-theme(
    text = element_text(size = 12, color = "blue"),
    axis.text = element_text(size = 10, color = "mediumorchid"),
    axis.title = element_text(size = 14, color = "lightsalmon", face = "bold"),
    panel.grid = element_line(color = "gray", linetype = "dashed"),
    panel.background = element_rect(fill = "pink"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.background = element_rect(fill = "lightyellow", color = "orange"),
    plot.title.position = "panel",  
    plot.title = element_text(hjust = 0.5, size=24, face="bold"),
    panel.border = element_rect(color = "black", linetype = "dashed", fill = NA)
  )


color_maximal<-scale_colour_gradient2(
  low = "chartreuse",
  mid = "coral",
  high = "darkslategray1",
  midpoint = 20,
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour"
)

D+theme_maximal+color_maximal
