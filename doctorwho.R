library(httr)
library(rvest)
library(tidyverse)
library(janitor)
library(extrafont)
library(cowplot)

doctorwho <- content(GET("https://en.wikipedia.org/wiki/List_of_Doctor_Who_episodes_(2005%E2%80%93present)"))
loadfonts(device = "win")


tables <- html_table(doctorwho,fill=TRUE)

tables[[4]]
season1 <- tables[[4]] %>%
  select(story = Story, episode = Episode, title = Title, viewers = `UK viewers(millions)<U+200A>[8]`) %>%
  mutate(doctor = "9th Doctor")

season2 <- tables[[5]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "10th Doctor")

season3 <- tables[[6]]%>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "10th Doctor")

season4 <- tables[[7]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "10th Doctor")

season4a <- tables[[8]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "10th Doctor")
season5 <- tables[[9]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "11th Doctor")

season6 <- tables[[10]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "11th Doctor")
season7 <- tables[[11]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "11th Doctor")

season7a <- tables[[12]] %>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "11th Doctor")
season8 <- tables[[13]]%>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "12th Doctor")

season9 <- tables[[14]]%>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "12th Doctor")
season10 <- tables[[15]]%>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "12th Doctor")
season11 <- tables[[16]]%>%
  clean_names() %>%
  select(story,episode,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "13th Doctor")
season12 <- tables[[17]]%>%
  clean_names() %>%
  select(story = no_story,episode=no_inseries,title,viewers = uk_viewers_millions_8)%>%
  mutate(doctor = "13th Doctor")


who_combine <- rbind(
  season1,
  season2,
  season3,
  season4,
  season4a,
  season5,
  season6,
  season7,
  season7a,
  season8,
  season9,
  season10,
  season11,
  season12
) %>%
  filter(str_detect(viewers,"Special") == FALSE) %>%
  filter(str_detect(viewers,"Series") == FALSE) %>%
  filter(str_detect(viewers,"Part") == FALSE) %>%
  filter(str_detect(viewers,"TBD") == FALSE) %>%
  mutate(viewers = as.numeric(viewers)) %>%
  mutate(episodenum = row_number())


logo_file <- system.file("extdata", "~/doc9.png", package = "cowplot")

img <- jpeg::readJPEG("doc1.jpg")
g <- grid::rasterGrob(img, interpolate=TRUE)

img1 <- jpeg::readJPEG("10thdoc.jpg")
g1 <- grid::rasterGrob(img1, interpolate=TRUE)

img2 <- jpeg::readJPEG("11thdoc.jpg")
g2 <- grid::rasterGrob(img2, interpolate=TRUE)

img3 <- jpeg::readJPEG("12thdoc.jpg")
g3 <- grid::rasterGrob(img3, interpolate=TRUE)

img4 <- jpeg::readJPEG("13thdoc.jpg")
g4 <- grid::rasterGrob(img4, interpolate=TRUE)

png("doctorwho.png",res = 300)
dev.off()
ggplot(who_combine, aes(x=episodenum, y = viewers, color = doctor)) +
  geom_path(size = .75) +
  geom_smooth(method = "lm", se = FALSE,size = 1.5) +
  ggsci::scale_color_futurama(breaks = c("9th Doctor","10th Doctor","11th Doctor","12th Doctor","13th Doctor")) +
  scale_y_continuous(limits = c(0,max(who_combine$viewers))) +
  theme_minimal() +
  annotation_custom(g, xmin=-5, xmax=25, ymin=2, ymax=5) +
  annotation_custom(g1, xmin=25, xmax=55, ymin=2.5, ymax=5.5) +
  
  annotation_custom(g2, xmin=65, xmax=95, ymin=2.5, ymax=5.5) +
  
  annotation_custom(g3, xmin=110, xmax=140, ymin=1.5, ymax=4.5) +
  annotation_custom(g4, xmin=145, xmax=175, ymin=1, ymax=4) +
  
  theme(
    plot.subtitle = element_text( face = "plain", hjust = 0.5, size = 16),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    #legend.spacing.x = unit(.5, 'cm'),
    #legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold")
    ) +
  labs(title = "Doctor Who UK Viewers",subtitle="With Linear Regression Line by Doctor",y = "Millions of Viewers",x="Epsode Number")


