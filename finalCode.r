library(ggplot2)  # Used to plot different graphs.
library(dplyr)  #Used for data manipulation.
library(statsr) #Used for sampling distributions , hypothesis testing etc.,
library(gridExtra) #Merge two graphs
library(corrplot) #Used to plot the graph for correlation matrix.

df=read.csv("chessAnalysis.csv",header=T,sep=",")
dim(df)
df=df[!duplicated(df),]
dim(df)
> sum(is.na(df[]))
> for(i in length(df[])){if(is.na(df[][i])) df[][i]=df[][i-1]}

> sum(is.na(df[]))

#Plot 1
ggplot(chess, aes(x = white_rating, y = black_rating, color = winner, shape = winner))+    geom_point(alpha = 0.5)+    theme_minimal()+    theme(legend.position = "bottom")+    scale_color_brewer(palette = 16)+    labs(y = "Black Rating", x = "White Rating", color = "Winner", shape = "Winner")

#Plot2
ggplot(chess, aes(x = white_rating, y = black_rating, color = winner, shape = winner))+    geom_point(alpha = 0.5)+    theme_minimal()+    theme(strip.background =element_rect(fill="gray"),          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"),          legend.position = "bottom")+    scale_color_brewer(palette = 16)+    facet_wrap(winner~.)+    labs(y = "Black Rating", x = "White Rating", color = "Winner", shape = "Winner")


#Plot 3
ggplot(chess, aes(x = white_rating, y = black_rating, color = winner, shape = winner))+    geom_smooth()+    theme_minimal()+    labs(y = "Black Rating", x = "White Rating", color = "Winner")


#Plot 4
chess  %>% count(victory_status) %>%     ggplot(aes(reorder(victory_status,n),n, fill = victory_status))+    geom_col(show.legend = FALSE)+    scale_fill_viridis_d()+    theme_minimal()+    coord_flip()+    labs(x = "Victory Status", y = "Frequency")


#Plot 5
ggplot(chess, aes(victory_status, fill = winner))+    geom_bar(position = "dodge")+    scale_fill_viridis_d()+    theme_minimal()+    theme(legend.position = "bottom")+    coord_flip()+    labs(x = "Victory Status", y = "Frequency", fill = "Winner")



#Plot 6
chess  %>% count(opening_name, sort = TRUE) %>% head(30) %>%     ggplot(aes(reorder(opening_name,n), n))+    geom_col(fill = "royalblue")+    coord_flip()+    theme_minimal()+    labs(y = "Frequency", x = "Opening Name")



#Plot 7
chess  %>% group_by(winner) %>% count(opening_name, sort = TRUE) %>% head(30) %>%     ggplot(aes(reorder(opening_name,n), n, fill = winner, color = winner))+    geom_col(position = "fill")+    coord_flip()+    theme_minimal()+    scale_fill_manual(values = c("black", "cornsilk"))+    scale_color_manual(values = c("cornsilk", "black"))+    labs(y = "Winning Ratio", x = "Opening Name", fill = "Winner")+    guides(color = FALSE)+    theme(legend.position = "bottom")



#Plot 8
chess  %>% group_by(opening_name, winner) %>%     summarise(mean_moves = mean(moves_length)) %>%     arrange(-mean_moves) %>%     ungroup() %>%     group_by(winner) %>%    top_n(10)  %>%     ggplot(aes(reorder(opening_name, mean_moves), mean_moves, fill = winner)) +    geom_col(show.legend = FALSE)+    coord_flip()+    facet_wrap(~winner, scales = "free_y", ncol = 1) +    theme_minimal()+    theme(strip.background =element_rect(fill="gray"),          strip.text.x = element_text(size = 10, colour = "white",face = "bold.italic"))+    labs(x = "Opening Name", y = "Mean Moves")+    scale_fill_manual(values = c("black", "steelblue", "cornsilk2"))



#Plot 9
grid.arrange(        ggplot(chess,aes(black_rating))+        geom_histogram(fill = "black")+        theme_minimal()+        labs(x= "Black Rating"),    ggplot(chess,aes(white_rating))+        geom_histogram(fill = "cornsilk")+        theme_minimal()+        labs(x= "White Rating"),        ggplot(data.frame(rating = c(chess$white_rating, chess$black_rating)),aes(rating))+        geom_histogram(fill = "steelblue")+        theme_minimal()+        labs(x= "Rating"))


>xbar=mean(df$turns)
>sigm=sd(df$turns)
> sigm
> n=600
> mu=mean(df$turns[1:600])
>z = (xbar - mu)/(sigm/sqrt(n))
> z

> df=df[1:500,]
> scatter.smooth(x=df$white_rating, y=df$black_rating, main="White_Rating ~ Black_Rating")

> boxplot(df$black_rating, main="Black Rating")
> boxplot(df$white_rating, main="White Rating")

> cor(df$white_rating,df$black_rating)
> black_outliers=boxplot(df$black_rating, main="Black Rating")$out
> black_outliers
numeric(0)
> white_outliers=boxplot(df$white_rating, main="White Rating")$out
> white_outliers


> df <- df[-which(df$white_rating %in% white_outliers),]
> boxplot(df$white_rating, main="White Rating")
> cor(df$white_rating,df$black_rating)

> linearmod = lm(white_rating ~ black_rating,data=df)
> linearmod

> summary(linearmod)

> trainingRowIndex = sample(1:nrow(df),0.8*nrow(df))
> trainingData = df[trainingRowIndex, ]
> testData = df[-trainingRowIndex, ]

> lmMod = lm(white_rating ~ black_rating,data=trainingData)
> ratingPred = predict(lmMod,testData)

> actualPreds = data.frame(cbind(actuals=testData$black_rating,predicteds=ratingPred))
> correlation_accuracy <- cor(actualPreds)

> head(actualPreds)

> min_max_accuracy <- mean(apply(actualPreds, 1, min) / apply(actualPreds, 1, max))
> min_max_accuracy

> cvResults <- suppressWarnings(CVlm(data=df,form.lm=black_rating ~ white_rating, m=5, dots=FALSE, seed=29, legend.pos="topleft"));






