setwd("C:/Anna/latent_attraction/writeup")
#setwd("C:/Users/Anna/Desktop/latent_attraction/writeup")
rm(list = ls())
library(data.table)
library(ggplot2)
library(lme4)
library(stargazer)



demonstrate <- data.table("Option_Label" = c("A", "B", "C"), "Location" = c(100, 20, 90),
                          "Price" = c(140, 100, 165), "Ticks_y" = c("Excellent", "Poor", "Good"),
                          "Ticks_x" = c("Medium (£140)", "Low (£100)","High (£165)"))


ggplot(demonstrate, aes(Price, Location, label= Option_Label)) + geom_point() + #scale_x_reverse(lim=c(165,100)) +
  geom_label(size = 7) + xlim(c(165, 85)) + scale_x_continuous(trans = "reverse", breaks = unique(demonstrate$Price),
                labels = c("Medium (£140)","Low (£100)","High (£165)"),limits = c(180, 85)) +
  scale_y_continuous(breaks = unique(demonstrate$Location), labels = c("Very good","Poor","Good"),limits = c(0,120)) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(size = 22))


# ggplot(demonstrate, aes(Price, Location, label= Option_Label)) + geom_point() + #scale_x_reverse(lim=c(165,100)) +
#   geom_label(size = 7) + xlim(c(165, 85)) + scale_x_continuous(breaks = unique(demonstrate$Price),
#                                                                labels = c("Medium (£140)","Low (£100)","High (£165)"),limits = c(85, 180)) +
#   scale_y_continuous(breaks = unique(demonstrate$Location), labels = c("Very good","Poor","Good"),limits = c(0,120)) +
#   theme(panel.grid.minor = element_blank()) +
#   theme(text = element_text(size = 22))
ggsave('exp1_intro.png')






load("C:/Anna/latent_attraction/Anna_scraped_movies/movie_lsas.RData")
#load("C:/Users/Anna/Desktop/latent_attraction/Movies_2ndgo/movie_lsas.RData")


#################plotting the most important dimensions#######################
latent_dims <- data.table(lsa_20dims_all[[2]])

#latent_dims[, importance := rank(-abs(value)), by = Var2]
latent_dims <- latent_dims[id <= 20,]
#latent_dims[, labeltext := paste(Var1, round(value,2), collapse = " "), by = 1:nrow(latent_dims)]
#latent_dims <- rbind(latent_dims, data.table("Var1" = paste("Dim no", 1:20), "Var2" = 1:20, 
#                                             "value" = 0, "id" = 0))


addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

ggplot(latent_dims[id <= 10 & Var2 <= 10,], aes(x= as.factor(Var2), y = id, fill = value, label= Var1)) +
  geom_tile()+ geom_text(colour = "black", size = 3.5,angle = 40) +  scale_y_reverse()+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "orange") +
  labs(x = "Latent dimension number", fill = "loading") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank())



library(ggplot2)
library(gridExtra)

p1 <- ggplot(latent_dims[id <= 10 & Var2 <= 10,], aes(x= as.factor(Var2), y = id, fill = value, label= Var1)) +
  geom_tile()+ geom_text(colour = "black", size = 3.5,angle = 26) +  scale_y_reverse()+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "orange") +
  labs(fill = "Loading") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank())
p2 <- ggplot(latent_dims[id <= 10 & Var2 > 10,], aes(x= as.factor(Var2), y = id, fill = value, label= Var1)) +
  geom_tile()+ geom_text(colour = "black", size = 3.5,angle = 26) +  scale_y_reverse()+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),axis.ticks.x=element_blank()) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "orange") +
  labs(x = "Latent dimension number", fill = "Loading") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank())








library(ggpubr)
ggarrange(p1, p2, nrow=2, common.legend = TRUE, legend="right")+ guides(fill=guide_legend(title="New Legend Title"))

ggsave("latent_dims.png")

#Neil doesn't like the picture, so table
# table_data <- latent_dims[id <= 10 & Var2 <= 20,]
# table_data[, label := paste(Var1 , round(value,2)), by = 1:nrow(table_data)]
# table_data <- dcast(table_data,id ~ Var2, value.var = "label")
# setnames(table_data, c("id", as.character(seq(1,20))),
#         c("rank",paste("LD number", as.character(seq(1,20)))))
# library(kableExtra)
# 
# table_data %>%
#   mutate_all(linebreak) %>%
#   kable("latex", booktabs = T, escape = F,
#         caption = "Main Title\\\\Subtitle",
#         col.names = linebreak(colnames(table_data), align = "c")) %>%
#   column_spec(1:21, width = "5cm")
# 
# 
# 
# kable(table_data, format = "latex", booktabs = T, kable_styling(column_spec(1:21, width = "5cm")))
# save_kable(kable(table_data, format = "latex"), file = "C:\\Anna\\latent_attraction\\table.tex")

###############plotting some movies############
movies <- data.table(lsa_20dims_all[[1]])

sample <- movies[c(1,7,1599,49),]

sample <- melt(sample, id.vars =  "title", measure.vars = paste("V", 1:20, sep = ""))
#sample <- melt(sample, id.vars =  "title", measure.vars = paste(1:20, sep = ""))

sample[, variable := factor(gsub("V", "", variable), levels = paste(1:20, sep = ""))]
sample <- sample[order(title, abs(value))]
sample[, Rank := rep(rev(c(1:20)), 4)]
sample <- sample[order(title, rank)]

table_data <- sample
#table_data[, value := round(value, 3)]

#table_data[, value := cell_spec(value > 0 , "orange", "blue")]
#table_data[, value :=ifelse(value>0,paste("\\color{orange}{",value, "}"),paste("\\color{blue}{",value, "}"))]
table_data[, dim :=ifelse(value>0,paste("\\color{orange}{",variable, "}"),paste("\\color{blue}{",variable, "}")), by = 1:nrow(table_data)]

#table_data <- dcast(sample, variable~title)

table_data <- dcast(table_data, Rank~title, value.var = "dim")
#setnames(table_data, "variable", "Latent dimension")

library(kableExtra)
#column_spec(kable_styling(kable(table_data, format = "latex")),1:21, width = "1cm")
bah <- row_spec(kable(table_data, format = "latex", booktabs = T, escape = F, align=rep('c', 5)), 0, bold = TRUE)
bah <- column_spec(bah, 1:5, width = "3cm")

ggplot(sample, aes(variable, value)) + geom_bar(stat = "identity", fill = "darkslateblue") + facet_wrap(~title, ncol = 1) +
  labs(x="Latent dimension number", y = "Latent dimension loading")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(strip.background = element_rect(fill="burlywood1"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank())

######movie distance###################
Documents<- data.table(lsa_20dims_all[[1]][movie.id %in% chosen_ones[,Movie.id],])
Documents[title == "Le cinquième élément", title := "The Fifth Element"]

distancematrix <- merge(Documents,data.table(title = chosen_ones[,Title], id = 1:200), by = "title", all.x = TRUE)
distancematrix <- data.table(distancematrix[order(id)])

distmat <- as.matrix(dist(distancematrix[,3:(ncol(distancematrix)-1)]))
rownames(distmat) <- distancematrix[,title]
colnames(distmat) <- distancematrix[,title]


plot.distance <- data.table("Title" = c(names(sort(distmat[143,])[c(1:6, 195:200)]),names(sort(distmat[183,])[c(1:6, 195:200)]),names(sort(distmat[66,])[c(1:6, 195:200)])),
                            "Distance" = c(as.numeric(sort(distmat[143,])[c(1:6, 195:200)]),as.numeric(sort(distmat[183,])[c(1:6, 195:200)]),as.numeric(sort(distmat[66,])[c(1:6, 195:200)])),
                            "ids" = rep(1:3,each = 12), "Nos" = rep(1:12, 3))

plot.distance_tab <- data.table("Title" = c(names(sort(distmat[143,])[c(1:6, 196:200)]),names(sort(distmat[183,])[c(1:6, 196:200)]),names(sort(distmat[66,])[c(1:6, 196:200)]),names(sort(distmat[62,])[c(1:6, 196:200)])),
                            "Distance" = c(as.numeric(sort(distmat[143,])[c(1:6, 196:200)]),as.numeric(sort(distmat[183,])[c(1:6, 196:200)]),as.numeric(sort(distmat[66,])[c(1:6, 196:200)]),as.numeric(sort(distmat[62,])[c(1:6, 196:200)])),
                            "ids" = rep(1:4,each = 11), "Nos" = rep(1:11, 4))

plot.distance_tab[, Title := gsub(": Episode IV - A New Hope|: An Unexpected Journey|: The Fellowship of the Ring",
                                  "", Title)]
plot.distance_tab[, Type := rep(c("Target", paste("Closest", 1:5), paste("Furthest", rev(1:5))), 4)]
plot.distance_tab[, Type := factor(Type, levels = c("Target", paste("Closest", 1:5), paste("Furthest", rev(1:5))),
                                   labels = c("Target", paste("Closest", 1:5), paste("Furthest", rev(1:5))))]
plot.distance_tab <- dcast(plot.distance_tab, Type~ids, value.var = "Title")
plot.distance_tab[, Type := factor(Type, levels = c("Target", paste("Closest", 1:5), paste("Furthest", rev(1:5))),
                                   labels = c("Target", paste("Closest", 1:5), paste("Furthest", rev(1:5))))]

setnames(plot.distance_tab, colnames(plot.distance_tab), c("Target", as.character(unlist(plot.distance_tab[1,2:5]))))

bah <- row_spec(kable(plot.distance_tab[-1,], format = "latex", booktabs = T,
                      escape = F, align=rep('c', 11), linesep = c("","","","","\\midrule","",
                                                                  "","","","","")), 0, bold = TRUE)

bah <- column_spec(bah, 2:5, width = "3cm")



addline_format <- function(x,...){
  gsub(':',':\n',x)
}


ggplot(plot.distance, aes(x= as.factor(ids), y = Nos, fill = Distance, label= addline_format(Title))) +
  geom_tile()+ geom_text(colour = "black", size = 4.3) +  scale_y_reverse()+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),axis.ticks.x=element_blank(),axis.text.x=element_blank()) +
  scale_fill_gradient2(low = "cornflowerblue", mid = "white", high = "orange") +
  #???labs(x = "Latent dimension number", fill = "loading") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank())
ggsave('distances.png')



#load("C:/Anna/latent_attraction/Anna_scraped_movies/genres.RData")
load("C:/Users/Anna/Desktop/latent_attraction/Movies_2ndgo/genres.RData")
alldata[, romance := "Romance" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, drama := "Drama" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, scifi := "Sci-Fi" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, thriller := "Thriller" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, comedy := "Comedy" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, horror := "Horror" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, adventure := "Adventure" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, mystery := "Mystery" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, animation := "Animation" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, fantasy := "Fantasy" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, crime := "Crime" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, action := "Action" %in% unlist(Categories), by = 1:nrow(alldata)]
#alldata[, doc := "Documentary" %in% unlist(Categories), by = 1:nrow(alldata)]
#alldata[, short := "Short" %in% unlist(Categories), by = 1:nrow(alldata)]
#alldata[, music := "Music" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, sport := "Sport" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, family := "Family" %in% unlist(Categories), by = 1:nrow(alldata)]
#alldata[, musical := "Musical" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, biography := "Biography" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, war := "War" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, history := "History" %in% unlist(Categories), by = 1:nrow(alldata)]
alldata[, western := "Western" %in% unlist(Categories), by = 1:nrow(alldata)]
genre_corr <- cor(matrix(as.numeric(as.matrix(alldata[,c(19:30,34:35,41,38:40)])), ncol = ncol(alldata[,c(19:30,34:35,41,38:40)])))
rownames(genre_corr) <- colnames(alldata[,c(19:30,34:35,41,38:40)])
colnames(genre_corr) <- colnames(alldata[,c(19:30,34:35,41,38:40)])


ggcorrplot(genre_corr, method = "circle") +  theme(axis.text=element_text(size=20),
                                                   axis.title=element_text(size=20))
library(ggcorrplot)

ggcorrplot(genre_corr, method = "circle",tl.cex = 15)
ggsave('correlation.png')



###################################### EXPERIMENT 1 #########################################


rm(list=ls())
load("C:/Anna/latent_attraction/Anna_scraped_movies/final_data.RData")
#load("C:/Users/Anna/Desktop/latent_attraction/Anna_scraped_movies/final_data.RData")
setwd("C:/Anna/latent_attraction/writeup")
#setwd("C:/Users/Anna/Desktop/latent_attraction/writeup")
choices_distr <- data.table("how.many" = triplets[,.N, by = worker.id][,N], "subid" = 1:101,
                            "decoy_share" =triplets[,sum(Which.chosen == "Decoy")/.N, by = worker.id][,V1])

ggplot(choices_distr, aes(how.many)) + geom_histogram(bins = 39, fill = "navy") + labs(x= "Number of choice trials", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20)) + scale_x_continuous(breaks = seq(1,40, by = 6)-1, limits = c(0,38))
ggplot(choices_distr, aes(how.many)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 1, fill = "navy") + labs(x= "Number of choice trials", y = "Proportion of participants") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20)) + scale_x_continuous(breaks = seq(1,40, by = 6)-1, limits = c(0,38))

ggsave('exp1_hist.png')


ggplot(choices_distr, aes(decoy_share)) + geom_histogram(binwidth = 0.01, fill = "navy") + labs(x= "Proportion of trials where the decoy was chosen", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))
ggplot(choices_distr, aes(decoy_share)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.008, fill = "navy") + labs(x= "Proportion of trials where the decoy was chosen", y = "Proportion of participants") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))
ggsave('exp1_decoytrials.png')

triplets[, Targetchosen := ifelse(Which.chosen == "Target",1,0)]

triplets[Quad_strict != TRUE & Which.chosen != "Decoy", betterhalf := rank(Combined_dec_dist)/.N]
#triplets[Quad_strict != TRUE  & Which.chosen != "Decoy", betterhalf := 0]
triplets[Quad_strict != TRUE & Which.chosen != "Decoy", Add_better := ifelse(betterhalf <= 0.5, TRUE, FALSE) ]

choices_distr[, Strictonly := triplets[Quad_strict == TRUE & Which.chosen != "Decoy",][,sum(Targetchosen)/.N, by = worker.id][,V1]]
choices_distr[, Betterhalftoo := triplets[(Quad_strict == TRUE  | Add_better == TRUE) & Which.chosen != "Decoy",][,sum(Targetchosen)/.N, by = worker.id][,V1]]

#choices_distr[, Betterhalf := triplets[betterhalf <= 0.5,][,sum(Targetchosen)/.N, by = worker.id][,V1]]



choices_distr <- melt(choices_distr, id.vars = "subid", measure.vars = c("Strictonly", "Betterhalftoo"))

choices_distr[, variable := factor(ifelse(variable == "Betterhalftoo", "Strict and better half", "Strict only"), 
                                   levels = c("Strict only", "Strict and better half"))]
library(simpleboot)
# choices_distr[variable == "Strict + better half", variable := "Strict and better half"]
# choices_distr[, variable := factor(variable, levels = c("Strict only", "Strict and better half"))]

cis <- data.table(variable = c("Strict only", "Strict and better half"),
                  value = 1, Lower = 1, Upper = 1)

pp <- one.boot(choices_distr[variable == cis[1,variable], value], mean, R=10^4)
cis[1, value := pp$t0]
pp <- boot.ci(pp, type = "perc")
cis[1, Lower := pp$percent[,4]]
cis[1, Upper := pp$percent[,5]]

pp <- one.boot(choices_distr[variable == cis[2,variable], value], mean, R=10^4)
cis[2, value := pp$t0]
pp <- boot.ci(pp, type = "perc")
cis[2, Lower := pp$percent[,4]]
cis[2, Upper := pp$percent[,5]]




ggplot(choices_distr, aes(variable, value)) + geom_point(position = "jitter", aes(colour = as.factor(subid))) +
  geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper, color = "firebrick"), width = 0.1)+
  stat_summary(fun.y = mean, color = "firebrick", geom = "point", size = 5, shape = 20)+ 
  theme(text = element_text(size=18), axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 18)) + scale_x_discrete(labels = c("High-quality only",
                                                                             "High-quality and better half")) +
  labs(y = "Proportion of trials where the decoy was chosen", x = "") + ylim(c(0,1))
ggsave('exp1_t_test.png')



ggplot(choices_distr[variable == "Strict only",], aes(variable, value)) +
  geom_point(position = "jitter") +
  geom_errorbar(data = cis[variable == "Strict only",], aes(ymin = Lower, ymax = Upper), width = 0.05, size = 1.2,colour = "firebrick")+
  geom_point(data = data.table(value = mean(choices_distr[variable == "Strict only",value]), variable = "Strict only"), aes(variable, value),colour = "firebrick", size = 3) +
  theme(text = element_text(size=20), axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 18)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(y = "Proportion of trials where the target was chosen", x = "") + ylim(c(0,1)) +
  geom_hline(aes(yintercept = 0.5),  linetype="dashed")
ggsave('exp1_highqual.png')


ggplot(choices_distr[variable == "Strict and better half",], aes(variable, value)) +
  geom_point(position = "jitter") +
  geom_errorbar(data = cis[variable == "Strict and better half",], aes(ymin = Lower, ymax = Upper), width = 0.05, size = 1.2,colour = "firebrick")+
  geom_point(data = data.table(value = mean(choices_distr[variable == "Strict and better half",value]), variable = "Strict and better half"), aes(variable, value),colour = "firebrick", size = 3) +
  theme(text = element_text(size=20), axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        legend.position = "none", legend.title = element_blank(),
        legend.text = element_text(size = 18)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(y = "Proportion of trials where the target was chosen", x = "") + ylim(c(0,1))+
  geom_hline(aes(yintercept = 0.5),  linetype="dashed")
ggsave('exp1_highqualplusbetter.png')




p1 <- ggplot(choices_distr, aes(x=value)) + geom_histogram(color="black", fill="royalblue") +
  labs(x="proportion of trials where the target was chosen", y = "frequency") + facet_wrap(~variable)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank())+
  xlim(c(0,1))

t.test(choices_distr[variable == "Strict only",value], alternative = "greater", conf.level=0.95, mu = 0.5)
t.test(choices_distr[variable == "Strict + better half",value], alternative = "greater", conf.level=0.95, mu = 0.5)

#https://stats.stackexchange.com/questions/263516/how-to-calculate-confidence-intervals-for-ratios

choices_distr[, prob_logits := log(value/(1-value))]
strict_reg1 <- summary(lm(data = choices_distr[variable == "Strict only",], prob_logits ~ 1))
better_half2 <- summary(lm(data = choices_distr[variable == "Strict + better half",], prob_logits ~ 1))

t_test.plotting <- data.table("type" = factor(c("Strict only", "Strict + better half"), levels = c("Strict only", "Strict + better half")),
                              "mean" = c(exp(strict_reg1$coefficients[1])/(1+exp(strict_reg1$coefficients[1])),
                                         exp(better_half2$coefficients[1])/(1+exp(better_half2$coefficients[1]))),
                              "lower_ci" = c(exp(strict_reg1$coefficients[1] - 1.96 * strict_reg1$coefficients[2])/(1+exp(strict_reg1$coefficients[1]- 1.96 * strict_reg1$coefficients[2])),
                                             exp(better_half2$coefficients[1] - 1.96 * better_half2$coefficients[2])/(1+exp(better_half2$coefficients[1]- 1.96 * better_half2$coefficients[2]))),
                              "upper_ci" = c(exp(strict_reg1$coefficients[1] + 1.96 * strict_reg1$coefficients[2])/(1+exp(strict_reg1$coefficients[1]+ 1.96 * strict_reg1$coefficients[2])),
                                             exp(better_half2$coefficients[1] + 1.96 * better_half2$coefficients[2])/(1+exp(better_half2$coefficients[1]+ 1.96 * better_half2$coefficients[2]))))

p2 <- ggplot(t_test.plotting, aes(as.factor(type), mean)) + geom_bar(stat = "identity", width = 0.4, fill = "royalblue") + ylim(c(0,1))+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) + labs(x = "", y = "proportion of trials where the target was chosen") 

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2)
ggsave('exp1_t_test.png')

triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]


triplets[, Comp_sem_scaled := scale(Comp_semantic_prox), .(worker.id)]
triplets[, Dec_sem_scaled := scale(Decoy_semantic_prox), .(worker.id)]
triplets[, Decoy_genre_prox_scaled := scale(Decoy_genre_prox), .(worker.id)]
triplets[, Comp_genre_prox_scaled := scale(Comp_genre_prox), .(worker.id)]


m1 <- glmer(Targetchosen ~ Allseen + Comp_sem_scaled  + Dec_sem_scaled +
              Decoy_genre_prox_scaled + Comp_genre_prox_scaled +
              Target_decoy_ratingdiff +
              (1 |  worker.id), data = triplets, family = binomial(link='logit'))

# summary(m1)
# 
 CI.vector <- exp(confint(m1))
 OR.vector <- exp(m1@beta) 
 p.values <- summary(m1)$coefficients[,4]
# 
# m2 <- glm(Targetchosen ~ Similarity_dec + Similarity_comp, data = triplets, family = binomial)
# 
# stargazer(m1,m2, type = "latex",ci=TRUE,single.row = TRUE,
#           coef = list(as.numeric(c(OR.vector)),as.numeric(exp(m2$coefficients))),
#           ci.custom = list(CI.vector, exp(confint(m2))),
#           p =list(p.values,summary(m2)$coefficients[,4]),
#           covariate.labels = c("Seen all", "Competitor distance",
#           "Decoy distance", "Decoy rating difference","Decoy similarity",
#           "Competitor similarity", "Intercept"),
#           dep.var.labels = "Target chosen proportion",
#           title = "Odds-ratios from two logistic models, Experiment 1.",
#           label = "latentattr_exp1reg",
#           out="C:/Anna/latent_attraction/writeup/exp1_regression.tex")

stargazer(m1, type = "latex",ci=TRUE,single.row = TRUE,
          coef = list(as.numeric(c(OR.vector))),
          ci.custom = list(CI.vector),
          p =list(p.values),
          covariate.labels = c("Seen all", "TC semantic dist", "TD semantic dist",
                               "TC genre dist", "TD genre dist",
                                "TD rating difference", "Intercept"),
          dep.var.labels = "Target chosen",
          title = "Odds-ratios from two logistic models, Experiment 1.",
          label = "latentattr_exp1reg",
          out="C:/Anna/latent_attraction/writeup/exp1_regression.tex")


check <- triplets[!is.na(Similarity_dec),c("Similarity_dec","Targetchosen")]

check[, list(prop = sum(Targetchosen == 1)/.N) ,.(Similarity_dec)]


cis <- data.table(Similarity_dec = 1:7, Mean = 1, Upper = 1, Lower = 1)
 for (i in 1:7) {
 pp <- one.boot(check[Similarity_dec == cis[i,Similarity_dec], Targetchosen], mean, R=10^4)
 cis[i, Mean := pp$t0]
 pp <- boot.ci(pp, type = "perc")
 cis[i, Lower := pp$percent[,4]]
 cis[i, Upper := pp$percent[,5]]
 }

library(merTools)
m1 <- glm(Targetchosen ~ Allseen + Comp_sem_scaled  + Dec_sem_scaled +
              Decoy_genre_prox_scaled + Comp_genre_prox_scaled +
              Target_decoy_ratingdiff, data = triplets, family = binomial(link='logit'))

# newdata <- data.table(expand.grid("Allseen" = TRUE,  
#                       Comp_sem_scaled = triplets[Comp_semantic_prox > 0.9,mean(Comp_sem_scaled)],
#                       Dec_sem_scaled = triplets[Decoy_semantic_prox == 0.01, mean(Dec_sem_scaled)],
#                       Decoy_genre_prox_scaled = triplets[Decoy_genre_prox == 1,mean(Decoy_genre_prox_scaled)], 
#                       Comp_genre_prox_scaled = triplets[Comp_genre_prox < 0.2,mean(Comp_genre_prox_scaled)],
#                       Target_decoy_ratingdiff = 3))
#                       #worker.id = unique(triplets$worker.id)))
# 

# newdata <- data.table(expand.grid("Allseen" = TRUE,  
#                                   Comp_sem_scaled = triplets[ Comp_semantic_prox == triplets[,max(Comp_semantic_prox)], mean(Comp_sem_scaled)],
#                                   Dec_sem_scaled = triplets[ Decoy_semantic_prox == triplets[,min(Decoy_semantic_prox)], mean(Dec_sem_scaled)],
#                                   Decoy_genre_prox_scaled = triplets[ Decoy_genre_prox == triplets[,max(Decoy_genre_prox)], mean(Decoy_genre_prox_scaled)],
#                                   Comp_genre_prox_scaled = triplets[ Comp_genre_prox == triplets[,min(Comp_genre_prox)], mean(Comp_genre_prox_scaled)],
#                                   Target_decoy_ratingdiff = 3, worker.id = unique(triplets$worker.id)))

newdata <- triplets[, list(Comp_sem_scaled = max(Comp_sem_scaled), Dec_sem_scaled = min(Dec_sem_scaled),
                Decoy_genre_prox_scaled = max(Decoy_genre_prox_scaled),
                Comp_genre_prox_scaled = min(Comp_genre_prox_scaled),
                Target_decoy_ratingdiff = max(Target_decoy_ratingdiff))]
newdata[, Allseen := TRUE]


baa <- predict(m1, newdata = newdata, type = "response", se.fit = TRUE, levels = 0.95)



predictInterval(merMod = m1, newdata = newdata,
                level = 0.95, n.sims = 10000,
                stat = "mean", type="probability",
                include.resid.var = 0)
# 
# merBoot <- bootMer(m1, predict, nsim = 100, re.form = NA)
# CI.lower = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.025, na.rm=TRUE)))
# CI.upper = apply(merBoot$t, 2, function(x) as.numeric(quantile(x, probs=.975, na.rm=TRUE)))
# 



predict.glm(m2, newdata = data.table(expand.grid(Similarity_comp = 1:7, Similarity_dec = 1:7)), type = "response", se.fit = TRUE)
check <- predict.glm(m2, newdata = data.table(expand.grid(Similarity_comp = 1:7, Similarity_dec = 1:7)), type = "response", se.fit = TRUE)
td_sim <- data.table(expand.grid(Similarity_comp = 1:7, Similarity_dec = 1:7))
td_sim[, Mean := check$fit]
td_sim[, Meanse := check$se.fit]
td_sim[, Lower := Mean - 2*Meanse]
td_sim[, Upper := Mean + 2*Meanse]
library(HH)
interval(m2, newdata = data.table(expand.grid(Similarity_comp = 1:7, Similarity_dec = 1:7)),
         type = "response",conf.level = 0.95)


#m2 <- glmer(Targetchosen ~ Allseen + Comp_semantic_prox +
#              Decoy_semantic_prox + Target_decoy_ratingdiff + Similarity_comp +
#              Similarity_dec + (1 |  worker.id), data = triplets, family = binomial)



#m3 <- glmer(Targetchosen ~ Allseen + Comp_semantic_prox  + Decoy_semantic_prox +
#              Target_decoy_ratingdiff + Similarity_comp +
#              (1 |  worker.id), data = triplets, family = binomial)


summary(m2)


stargazer(m1,m2, covariate.labels = c("Seen all", "Competitor distance", "Decoy distance", "Decoy rating difference",
                                         "Decoy similarity", "Competitor similarity", "Intercept"),
          dep.var.labels = "Target chosen",
          type = "latex", out="C:/Anna/latent_attraction/writeup/exp1_regression.tex",
          title = "Experiment 1 regression results",
          font.size = "footnotesize",
          label = "exp1_reg")


similarity <- rbind(data.table("rating" = triplets[!is.na(Similarity_comp),Similarity_comp], "type" = "Target-Competitor"),
                    data.table("rating" = triplets[!is.na(Similarity_dec),Similarity_dec], "type" = "Target-Decoy"))

ggplot(similarity, aes(rating)) + geom_histogram(fill= "royalblue", binwidth = 0.5) + facet_wrap(~ type, ncol = 1) +
  labs(x= "Similarity rating (1 = Least similar, 7 = Most similar)", y = "Frequency") +
  scale_x_continuous(breaks = 1:7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))

ggsave('exp1_similarityratings.png')




###################################### EXPERIMENT 2 #########################################

rm(list=ls())
setwd("C:/Anna/latent_attraction/writeup")
#setwd("C:/Users/Anna/Desktop/latent_attraction/writeup")
decoy_pairs <- data.table(read.csv("C://Anna//latent_attraction//Movies_2ndgo//targetdecoy_ratings.csv"))
#decoy_pairs <- data.table(read.csv("C://Users//Anna//Desktop//latent_attraction//Movies_2ndgo//targetdecoy_ratings.csv"))



ggplot(decoy_pairs, aes(mean)) + geom_histogram(fill="navy")

ggplot(decoy_pairs, aes(x=mean)) + geom_histogram(color="black", fill="royalblue") +
  labs(x="Mean similarity rating", y = "Frequency") + geom_vline(xintercept = 4.5,linetype="dotted", size = 1.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))
ggsave('exp2_pilot.png')

load("C:/Anna/latent_attraction/Movies_2ndgo/final_data_second.RData")
#load("C:/Users/Anna/Desktop/latent_attraction/Movies_2ndgo/final_data_second.RData")

choices_distr <- data.table("how.many" = triplets[,.N, by = worker.id][,N], "subid" = 1:length(unique(triplets$worker.id)),
                            "decoy_share" =triplets[,sum(Which.chosen == "Decoy")/.N, by = worker.id][,V1])


ggplot(choices_distr, aes(how.many)) + 
  geom_histogram(binwidth = 0.7, fill = "navy") + labs(x= "Number of choice trials", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20)) + scale_x_continuous(breaks = seq(1,60, by = 5)-1, limits = c(0,56))
ggplot(choices_distr, aes(how.many)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 1, fill = "navy") + labs(x= "Number of choice trials", y = "Proportion of participants") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20)) + scale_x_continuous(breaks = seq(1,56, by = 6)-1, limits = c(0,56))


ggsave('exp2_hist.png')


ggplot(choices_distr, aes(decoy_share)) + geom_histogram(binwidth = 0.01, fill = "navy") + labs(x= "Proportion of trials where the decoy was chosen", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))
ggplot(choices_distr, aes(decoy_share)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.008, fill = "navy") + labs(x= "Proportion of trials where the decoy was chosen", y = "Proportion of participants") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))
ggsave('exp2_decoytrials.png')




triplets[, Targetchosen := ifelse(Which.chosen == "Target",1,0)]
triplets[, Compchosen := ifelse(Which.chosen == "Competitor",1,0)]
choices_distr <- triplets[Which.chosen != "Decoy", sum(Targetchosen)/.N ,by = worker.id]

choices_distr[, variable := as.factor("baa")]
setnames(choices_distr, "V1", "value")

 cis <- data.table(value = 1, Lower = 1, Upper = 1)
# 
 pp <- one.boot(choices_distr[, value], mean, R=10^4)
 cis[,value := pp$t0]
 pp <- boot.ci(pp, type = "perc")
 cis[,Lower := pp$percent[,4]]
 cis[,Upper := pp$percent[,5]]
 cis[, variable := "baa"]
# 
 ggplot(choices_distr, aes(variable, value)) +
   geom_point(position = "jitter") +
   geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper), width = 0.05, size = 1.2,colour = "firebrick")+
   geom_point(data = data.table(value = mean(choices_distr[,value]), variable = "baa"), aes(variable, value),colour = "firebrick", size = 3) +
   theme(text = element_text(size=19), axis.text.x = element_text(size = 20),
         axis.text.y = element_text(size = 18),
         legend.position = "none", legend.title = element_blank(),
         legend.text = element_text(size = 18)) +
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank())+
   labs(y = "Proportion of trials where the target was chosen", x = "") + ylim(c(0,1)) +
   geom_hline(aes(yintercept = 0.5),  linetype="dashed")+ coord_fixed(1)
 ggsave("exp2_res.png")
# 
# 
# ggplot(choices_distr, aes(value)) + geom_point(position = "jitter") +
#   geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper, color = "firebrick"), width = 0.1)+
#   stat_summary(fun.y = mean, color = "firebrick", geom = "point", size = 5, shape = 20)+ 
#   theme(text = element_text(size=18), axis.text.x = element_text(size = 20),
#         axis.text.y = element_text(size = 16),
#         legend.position = "none", legend.title = element_blank(),
#         legend.text = element_text(size = 18)) +
#   labs(y = "Proportion of trials where the decoy was chosen", x = "") + ylim(c(0,1))

# choices_distr[, no := 1:.N]
# 
# 
# cis <- data.table("V1" = 1, "Upper" = 1, "Lower" = 1)
# 
# pp <- one.boot(choices_distr[,V1], mean, R=10^4)
# cis[, V1 := pp$t0]
# pp <- boot.ci(pp, type = "perc")
# cis[, Lower := pp$percent[,4]]
# cis[, Upper := pp$percent[,5]]
# cis[, no := 68]
# 
#  ggplot(choices_distr, aes(no, V1)) +
#    geom_point(position = "jitter") +
#   geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper), width = 0.05, size = 1.2,colour = "firebrick")+
#    geom_point(data = data.table(value = mean(choices_distr[,V1]), no = 68), aes(68, value),colour = "firebrick", size = 3) +
#    theme(text = element_text(size=20), axis.text.x = element_text(size = 20),
#          axis.text.y = element_text(size = 18),
#          legend.position = "none", legend.title = element_blank(),
#          legend.text = element_text(size = 18)) +
#    theme(axis.title.x=element_blank(),
#          axis.text.x=element_blank(),
#          axis.ticks.x=element_blank())+
#    labs(y = "Proportion of trials where the target was chosen", x = "") + ylim(c(0,1))+
#    geom_hline(aes(yintercept = 0.5),  linetype="dashed")
# 
# ggplot(choices_distr, aes(no, V1)) + geom_point(position = "jitter") +   
#   geom_errorbar(data = cis, aes(ymin = Lower, ymax = Upper),color = "firebrick", size = 1)+
#   geom_point(data = cis, aes(no, V1), colour = "firebrick", size = 3)+ 
#   theme(text = element_text(size=18),
#         axis.ticks.x=element_blank(),axis.text.x=element_blank(),
#         legend.position = "none", legend.title = element_blank(),
#         legend.text = element_text(size = 18)) +
#   labs(y = "Proportion of trials where the decoy was chosen", x = "") + ylim(c(0.15,0.75))



p1 <- ggplot(choices_distr, aes(x=V1)) + geom_histogram(color="black", fill="royalblue") +
  labs(x=" ", y = "Frequency") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank())+
  xlim(c(0,1))+
  theme(text = element_text(size = 15),
        axis.title.y = element_text(margin = margin(t = 0, r =60, b = 0, l =0)),
        axis.ticks.x=element_blank(),axis.text.x=element_blank())
#        +
#  theme(plot.margin=unit(c(0,1,0,4),"cm"),axis.ticks.x=element_blank(),axis.text.x=element_blank())
#ggsave('exp2_propdistr.png')


t.test(choices_distr[,V1], alternative = "greater", conf.level=0.95, mu = 0.5)


choices_distr[, prob_logits := log(V1/(1-V1))]
exp2_reg <- summary(lm(data = choices_distr, prob_logits ~ 1))


t_test.plotting <- data.table("mean" = exp(exp2_reg$coefficients[1])/(1+exp(exp2_reg$coefficients[1])),
                              "lower_ci" = exp(exp2_reg$coefficients[1] - 1.96 * exp2_reg$coefficients[2])/(1+exp(exp2_reg$coefficients[1]- 1.96 * exp2_reg$coefficients[2])),
                              "upper_ci" = exp(exp2_reg$coefficients[1] + 1.96 * exp2_reg$coefficients[2])/(1+exp(exp2_reg$coefficients[1]+ 1.96 * exp2_reg$coefficients[2])))
p2 <- ggplot(t_test.plotting, aes(1,mean)) + geom_bar(stat = "identity", width = 0.4, fill = "royalblue") + ylim(c(0,1))+ xlim(c(0.5,1.5)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) + labs(x = " ", y = "Proportion of trials where the target was chosen") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(text = element_text(size = 15))+
  theme(plot.margin=unit(c(0,1,0.5,1.5),"cm"))

library(grid)
library(dplyr)
#ggsave('exp2_bar.png')
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2 + coord_flip()), size = "last"))
ggsave("exp2_res.png")



triplets[, Similarity_comp := as.numeric(Similarity_comp)]
triplets[, Similarity_dec := as.numeric(Similarity_dec)]




similarity <- rbind(data.table("rating" = triplets[,Similarity_comp], "type" = "Target-Competitor"),
                    data.table("rating" = triplets[,Similarity_dec], "type" = "Target-Decoy"))

ggplot(similarity[rating != 8,], aes(rating)) + geom_histogram(fill= "royalblue", binwidth = 0.5) + facet_wrap(~ type, ncol = 1) +
  labs(x= "Similarity rating (1 = Least similar, 7 = Most similar)", y = "Frequency") +
  scale_x_continuous(breaks = 1:7)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_blank()) +
  theme(text = element_text(size = 20))

ggsave('exp2_similarityratings.png')


triplets[Similarity_comp == 8, Similarity_comp := NA]
triplets[Similarity_dec == 8, Similarity_dec := NA]

triplets[, Scaled_simcomp := scale(Similarity_comp), .(worker.id)]
triplets[, Scaled_simdec := scale(Similarity_dec), .(worker.id)]

triplets[, Target_decoy_ratingdiff := Target_rating - Decoy_rating]



m1 <- glmer(Targetchosen ~ Allseen + Scaled_simcomp  + Scaled_simdec +
              Target_decoy_ratingdiff +
              (1 |  worker.id), data = triplets, family = binomial)

summary(m1)

CI.vector <- exp(confint(m1))
OR.vector <- exp(m1@beta)
p.values <- summary(m1)$coefficients[,4]



stargazer(m1, type = "latex",ci=TRUE,single.row = TRUE,
          coef = list(as.numeric(c(OR.vector))),
          ci.custom = list(CI.vector),
          p =list(p.values),
          covariate.labels = c("Seen all", "TC similarity rating", "TD similarity rating",
                               "TD rating difference", "Intercept"),
          dep.var.labels = "Target chosen",
          title = "Odds-ratios from a logistic mixed-effects model, Experiment 2.",
          label = "latentattr_exp2reg",
          out="C:/Anna/latent_attraction/writeup/exp2_regression.tex")


#stargazer(m1, covariate.labels = c("Seen all", "Competitor similarity", "Decoy similarity", "Decoy rating difference","Intercept"),
#          dep.var.labels = "Target chosen",
#          type = "latex", out="C:/Anna/latent_attraction/writeup/exp2_regression.tex",
#          title = "Experiment 2 regression results",
#          font.size = "footnotesize",
#          label = "exp2_reg")

# cis <- exp(confint(m1))
# 
# stargazer(m1, covariate.labels = c("Seen all", "Competitor similarity", "Decoy similarity", "Decoy rating difference","Intercept"),
#           dep.var.labels = "Target chosen",ci=TRUE,single.row = TRUE,
#           coef = list(exp(m1@beta)),
#           ci.custom = list(cis),
#           p =list(summary(m1)$coefficients[,4]),
#           type = "latex", out="C:/Anna/latent_attraction/writeup/exp2_regression.tex",
#           title = "Odds-ratios from a mixed-effects logistic regression, Experiment 2.",
#           font.size = "footnotesize",
#           label = "latent_attr_reg2")



m1 <- glm(Targetchosen ~ Allseen + Scaled_simcomp  + Scaled_simdec +
              Target_decoy_ratingdiff, data = triplets, family = binomial)

newdata <- triplets[, list(Scaled_simcomp = min(Scaled_simcomp, na.rm = TRUE),
                           Scaled_simdec = max(Scaled_simdec, na.rm = TRUE),
                           Target_decoy_ratingdiff = max(Target_decoy_ratingdiff))]
newdata[, Allseen := TRUE]


baa <- predict(m1, newdata = newdata, type = "response", se.fit = TRUE, levels = 0.95)





