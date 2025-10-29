library(ggplot2)
library(cowplot)
library(magick)

# read data
df1 <- readRDS("WeAreRLadies/2021_December_13_18/poll_data/poll2.rds")

# make plot
p <- ggplot(df1, aes(x=response, y=percentages)) +
  geom_col(fill="#88398a") +
  geom_text(aes(label=response, y=percentages+1), hjust=0, colour = "#88398a", family="sans", size = 5) +
  scale_y_continuous(limits=c(0,60), breaks=seq(0,40,10)) +
  labs(x="", y="\n % of respondents",
       title = "Have you built your own R package?\n", 
       subtitle = "56 R users responded to a poll asking whether or not they had built their own R package. Almost\n20% of people said they would like some help to build their first R package.\n", 
       caption = "\nPoll: twitter.com/nrennie35/status/1471102160175865872") +
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
  draw_image(img, 0.41, -0.39, scale=0.2)
q

# save plot
ggsave(q, 
       filename="WeAreRLadies/2021_December_13_18/poll_plots/poll2.jpg", 
       width = 11, 
       height = 7,
       units="in")
