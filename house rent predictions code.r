library(ggplot2)
options(repr.plot.width = 12, repr.plot.height = 8)
data <- read.csv(file = 'C:/Users/hp/Downloads/data.csv')
head(data)
tail(data)
print(paste("Number of records: ", nrow(data)))
print(paste("Number of features: ", ncol(data)))
summary(data)
colnames(data) 
unique(data$city)
maindf <- data[,c("price","bedrooms","sqft_living","floors","sqft_lot", "condition", "view", "yr_built")]
head(maindf)
sum(is.na(maindf))
maindf$oldbuilt <- as.integer(format(Sys.Date(), "%Y")) - maindf$yr_built

drops <- c("yr_built")
maindf = maindf[ , !(names(maindf) %in% drops)]
cor(maindf)
library(ggcorrplot)
corr <- round(cor(maindf), 1)

# Plot
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 5,  
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of Housing Dataset", 
           ggtheme=theme_bw)
pairs(~bedrooms + sqft_living + floors + condition, data = maindf,
      main = "Scatterplot Matrix")
theme_set(theme_bw()) 
g <- ggplot(maindf, aes(bedrooms, floors))
g + geom_count(col="tomato3", show.legend=F) +
  labs(y="floors", 
       x="bedrooms", 
       title="Bedrooms vs Floors")
plot(x = maindf$sqft_living, y = maindf$sqft_lot,
     xlab = "sqft_living",
     ylab = "sqft_lot",
     xlim = c(0, 3000), 
     ylim = c(0, 20000),
     main = "sqft_living vs sqft_lot")
ggplot(maindf,aes(y=price,x=sqft_living)) +
  geom_point() + 
  xlim(0, 9000) +
  ylim(0, 5000000) +
  geom_smooth(formula = y ~ x,method="lm")
theme_set(theme_bw()) 
g <- ggplot(maindf, aes(bedrooms, floors))
g + geom_count(col="tomato3", show.legend=F) +
  labs(y="floors", 
       x="bedrooms", 
       title="Bedrooms vs Floors")
plot(x = maindf$sqft_living, y = maindf$sqft_lot,
     xlab = "sqft_living",
     ylab = "sqft_lot",
     xlim = c(0, 3000), 
     ylim = c(0, 20000),
     main = "sqft_living vs sqft_lot"
)
ggplot(maindf,aes(y=price,x=sqft_living)) +
  geom_point() + 
  xlim(0, 9000) +
  ylim(0, 5000000) +
  geom_smooth(formula = y ~ x,method="lm")
