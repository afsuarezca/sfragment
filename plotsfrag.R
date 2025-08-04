library(ggplot2)
dist_params<-data.frame(mean= seq(1,10,1),sd = 0.5)




  ggplot(data.frame(x = c(-4, 14)), aes(x = x)) +
    # stat_function(fun = dnorm, args = list(6, 0.9),colour="#A6C6E7",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
    # stat_function(fun = dnorm, args = list(7, 0.8),colour="#A6C6E7",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
    # stat_function(fun = dnorm, args = list(8, 0.7),colour="#A6C6E7",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
    # stat_function(fun = dnorm, args = list(9, 0.6),colour="#A6C6E7",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
    # stat_function(fun = dnorm, args = list(10, 0.5),colour="#A6C6E7",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
      stat_function(fun = dnorm, args = list(1, 1.7),colour="#3B681A",geom = "area",
                    size = 0, alpha = 0.5,fill ="#D7E9CB") +
    stat_function(fun = dnorm, args = list(2, 1.5),colour="#3B681A",geom = "area",
                  size = 0, alpha = 0.5,fill ="#D7E9CB") +
    stat_function(fun = dnorm, args = list(3, 1.1),colour="#3B681A",geom = "area",
                  size = 0, alpha = 0.5,fill ="#D7E9CB") +
     stat_function(fun = dnorm, args = list(4, 0.7),colour="#3B681A",geom = "area",
                   size = 0, alpha = 0.5,fill ="#D7E9CB") +
    stat_function(fun = dnorm, args = list(5, 0.5),colour="#3B681A",geom = "area",
                  size = 0, alpha = 0.5,fill ="#D7E9CB") +
    # stat_function(fun = dnorm, args = list(8, 1.7),colour="#1F4973",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
    # stat_function(fun = dnorm, args = list(9, 1.8),colour="#1F4973",geom = "area",
    #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
     # stat_function(fun = dnorm, args = list(10, 1.9),colour="#1F4973",geom = "area",
     #               size = 0, alpha = 0.5,fill ="#A6C6E7") +
    labs(#title = "Density Plot of 15 Normal Distributions Using stat_function",
         x = "",
         y = "") +
    theme_classic()+
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks = element_blank())
  
  