library(dplyr)
getwd()
compensation <- read.csv("Data/compensation.csv")
dim(compensation)
as_tibble(compensation)
glimpse(compensation)
select(compensation,Root,Fruit)
select(compensation,-Root) # removes a column
slice(compensation, 2:5)
slice(compensation, c(2,5))
select(slice(compensation, 2:5),Root,Fruit)
select(slice(compensation, 8),Fruit)

#piping
compensation %>% #pipe
  select(Root) %>%
  slice(8)

compensation %>%
  filter(Fruit > 80) %>%
  select(Root)

filter(compensation, Grazing == "Ungrazed")  
filter(compensation, Fruit > 80)  

compensation %>%
  summarise(meanF = mean(Fruit), sdF = sd(Fruit), ssF = length(Fruit))

summary(compensation)

compensation %>%
  group_by(Grazing) %>%
  summarise(meanF = mean(Fruit), sdF = sd(Fruit))

compensation %>%
  mutate(FRatio = Fruit/Root) %>%
  group_by(Grazing) %>%
  summarise(meanF = mean(Fruit), sdF = sd(Fruit))

compensation %>%
  mutate(FRatio = Fruit/Root) %>%
  group_by(Grazing) %>%
  summarise(medianF = median(FRatio), vF = var(FRatio), ssF = length(FRatio))

library(ggplot2)
ggplot(compensation, aes(x = Root, y = Fruit, colour = Grazing))+geom_point()

ggplot(compensation, aes(x = Root, y = Fruit, colour = Grazing, shape = Grazing))+
      geom_point()+
      scale_colour_manual(values = c("springgreen", "deeppink"))

colors()

ggplot(compensation, aes(x = Root, y = Fruit, colour = Grazing, shape = Grazing))+
  geom_point()+
  theme_bw() +
  facet_wrap(~Grazing)

ggplot(compensation, aes(x = Root, y = Fruit, colour = Grazing, shape = Grazing))+
  geom_point()+
  theme_bw() +
  facet_wrap(~Grazing, ncol =1)

ggplot(compensation, aes(x = Root))+
     geom_histogram(bins = 10, fill = "deeppink")

ggplot(compensation, aes(x = Root))+
  geom_histogram(bins = 10, fill = "deeppink")+
  facet_wrap(~Grazing, ncol =1)

ggplot(compensation, aes(x = Grazing, y= Fruit))+
  geom_boxplot(alpha = 0.8) # Transparency

ggplot(compensation, aes(x = Grazing, y= Fruit))+
  geom_boxplot(fill = "blue") +
  theme_light()

ggsave(mygraph, "myGraph.jpeg")

##########Have a great weekend, this has been fun!
