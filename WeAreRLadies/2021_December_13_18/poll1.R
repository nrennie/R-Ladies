library(ggplot2)
library(cowplot)
library(magick)

# read data
df1 <- readRDS("WeAreRLadies/2021_December_13_18/poll_data/poll1.rds")

# make plot
p <- ggplot(df1, aes(x=industry, y=percentages)) +
  geom_col(fill="#88398a") +
  geom_text(aes(label=industry, y=percentages+1), hjust=0, colour = "#88398a", family="sans", size = 5) +
  scale_y_continuous(limits=c(0,50), breaks=seq(0,40,10)) +
  labs(x="", y="\n % of respondents",
       title = "Which industry/research area do you work in?\n", 
       subtitle = "109 R users responded to a poll asking which industry or research area they work in. Most were\nin the life sciences, but other answers were wide ranging from financial services to assessment\ndevelopment.\n", 
       caption = "\nPoll: twitter.com/nrennie35/status/1470393652266606599") +
  coord_flip(expand = F) +
  theme(plot.background = element_rect(fill = "white", colour="white"),
        panel.background = element_rect(fill = "white", colour="white"),
        plot.title = element_text(colour = "#88398a", size=22, face="bold", family="sans", hjust=0),
        plot.subtitle = element_text(colour = "#88398a", size=14, family="sans"),
        plot.caption = element_text(colour = "#88398a", size=10, family="sans", hjust=0),
        axis.ticks.x = element_line(colour = "#88398a"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "#88398a", size=12, family="sans"),
        axis.title = element_text(colour = "#88398a", size=12, family="sans"),
        plot.margin = unit(c(0.8, 0.8, 0.3, 0.3), "cm"), #top, right, bottom, left
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
p

# add logo
img <- image_read("WeAreRLadies/2021_December_13_18/logo.png")
q <- ggdraw() + 
  draw_plot(p) +
  draw_image(img, 0.41, -0.35, scale=0.2)
q

# save plot
ggsave(q, 
       filename="WeAreRLadies/2021_December_13_18/poll_plots/poll1.jpg", 
       width = 11, 
       height = 7,
       units="in")
