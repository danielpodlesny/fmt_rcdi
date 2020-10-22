#### Custom Theme

theme_dp <- function(base_size=12, font='serif'){
  theme_classic(base_size = base_size, base_family = font) +
    theme(
      ## background
      # panel.background = element_rect(fill = "white", colour = "grey50")
    )
}


#### Custom Colors
pewter = '#88B7B5'
cambridge = '#A7CAB1'
hgold = '#DDA600'
steel = '#5F718B'
ucla = '#E8AE00'
wintergreen = '#478E80'
tuscan = '#6C434F'
gray = '#918D90'
lgray = '#B9B6B8'
flavescent = '#FFE28E'
flax = '#E8CE82'
ube = '#75375A'
liberty = '#4C4C9D'
gblue = '#8989B7'
elavender = '#B07797'

colors.custom = c(hgold, flax, liberty, gblue, wintergreen, cambridge, ube, elavender)

colors.main = c(wintergreen, hgold, liberty, ube, gray)
colors.light = c(cambridge, flax, gblue, elavender, lgray)
colors.continuous.two.main = c(wintergreen, hgold)
colors.continuous.two.light = c(cambridge, flax)
colors.continuous.three.main = c(wintergreen, hgold, liberty)
colors.continuous.three.light = c(cambridge, flax, gblue)
colors.discrete = c(colors.main, colors.light)
recipient = wintergreen
post = hgold
donor = liberty

df <- data.frame(xval=rnorm(50), continuous=rnorm(50), discrete=c('A','B','C','D','E'))

# Discrete Main
ggplot() + 
  geom_histogram(data = df, aes(x=xval, fill=discrete)) + 
  theme_dp() +  
  scale_fill_manual(values=colors.main)

# Discrete Light
ggplot() + 
  geom_histogram(data = df, aes(x=xval, fill=discrete)) +
  theme_dp() + 
  scale_fill_manual(values=colors.light)

# Continuous 2-F Main
ggplot(df, aes(x=xval, y=continuous, colour=continuous)) + 
  geom_point() + theme_dp() + 
  scale_colour_gradientn(colours=colors.continuous.two.main)

# Continuous 2-F Light
ggplot(df, aes(x=xval, y=continuous, colour=continuous)) + 
  geom_point() + theme_dp() + 
  scale_colour_gradientn(colours=colors.continuous.two.light)

# Continuous 3-F Main
ggplot(df, aes(x=xval, y=continuous, colour=continuous)) + 
  geom_point() + theme_dp() + 
  scale_colour_gradientn(colours=colors.continuous.three.main)

# Continuous 3-F Light
ggplot(df, aes(x=xval, y=continuous, colour=continuous)) + 
  geom_point() + theme_dp() + 
  scale_colour_gradientn(colours=colors.continuous.three.light)


## FROM
if(!requireNamespace("showtext", quietly = TRUE)){
  install.packages('showtext', dependencies = TRUE)
  install.packages("extrafont")
}
library(showtext)
font_add_google("Roboto", "Roboto")
font_add_google("Montserrat", "Montserrat")

theme_cm <- function(){
  theme_minimal() +
    theme(text = element_text(family = "sans-serif", size = 10, colour = "black"),
          axis.text = element_text(family = "sans-serif", size = 10, colour = "black"),
          strip.background = element_rect(colour = "transparent", fill = "transparent"),
          strip.text = element_text(family = "sans-serif", size = 12, colour = "black"))
}

cm_palette <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#FF8F33','#FFE133','#6D3100')
