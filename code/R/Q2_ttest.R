library(jsonlite)
library(ggplot2)

mean.list.phys <- jsonlite::read_json("~/Downloads/Q2_Physician_json.txt", simplifyVector = TRUE)
mean.list.nurse <- jsonlite::read_json("~/Downloads/Q2_Nurse_json.txt", simplifyVector = TRUE)
mean.list.nurse[[1]] <- NULL

totaldf.final <- NULL
totalmeandf.final <- NULL

for(mod.type in 1:2){
  mean.list <- list(mean.list.phys, mean.list.nurse)[[mod.type]]
  totaldf <- NULL
  totalmeandf <- NULL
  for(i in 1:length(mean.list)){
    df <- mean.list[[i]]
    if(is.null(dim(df))){
      next
    }
    dfmean <- colMeans(df)
    dfmean.melt <- data.frame(variable = colnames(df),
                              value = dfmean,
                              Year = names(mean.list)[i])
    if(is.null(totalmeandf)){
      totalmeandf <- dfmean.melt
    } else {
      totalmeandf <- rbind(totalmeandf, dfmean.melt)
    }
    
    df$Message <- paste0("Message", 1:nrow(df))
    df.melt <- reshape2::melt(df, id.vars = "Message")
    df.melt$Year <- names(mean.list)[i]
    if(is.null(totaldf)){
      totaldf <- df.melt
    } else {
      totaldf <- rbind(totaldf, df.melt)
    }
  }
  
  # add to total:
  totaldf$Provider <- c("Physician", "Nurse")[mod.type]
  totalmeandf$Provider <- c("Physician", "Nurse")[mod.type]
  
  if(is.null(totaldf.final)){
    totaldf.final <- totaldf
    totalmeandf.final <- totalmeandf
  } else {
    totaldf.final <- rbind(totaldf.final, totaldf)
    totalmeandf.final <- rbind(totalmeandf.final, totalmeandf)
  }
  
}



#totaldf <- dplyr::filter(totaldf, variable == "total")

g1 <- ggplot(totalmeandf.final) +
  geom_point(aes(x = Year, y = value, color = variable, group = variable)) +
  geom_line(aes(x = Year, y = value, color = variable, group = variable)) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "maroon", "orange",
                                "dark red", "dark blue", "dark green", "light blue", "light green", "tomato",
                                "grey", "black")) +
  theme_bw() +
  facet_wrap(vars(Provider), ncol = 1)

g2 <- ggplot(dplyr::filter(totalmeandf.final, variable != "total")) +
  geom_bar(aes(x = Year, y = value, fill = variable, group = variable), color = "black", stat = "identity") +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "maroon", "orange",
                                "dark red", "dark blue", "dark green", "light blue", "light green", "tomato",
                                "grey")) +
  theme_bw() +
  facet_wrap(vars(Provider), ncol = 1)

p <- cowplot::plot_grid(g2, g1)
p
cowplot::save_plot("~/Documents/CBBCoreTopics/Q2_freq.png", p, base_asp = 2.8)



for(mod.type in 1:2){
  # Perform t-tests:
  mean.list <- list(mean.list.phys, mean.list.nurse)[[mod.type]]
  pval.mat <- array(NA, dim = c(length(mean.list), length(mean.list), 14))
  rownames(pval.mat) <- names(mean.list)
  colnames(pval.mat) <- names(mean.list)
  for(i in 1:length(mean.list)){
    for(ii in 1:length(mean.list)){
      df1 <- mean.list[[i]]
      df2 <- mean.list[[ii]]
      for(j in 1:14){
        res <- t.test(df1[,j], df2[,j])$p.value
        pval.mat[i, ii, j] <- res
      }
    }
  }
  
  # Heatmap:
  pval.melt <- reshape2::melt(pval.mat)
  pval.melt$Var3 <- colnames(mean.list[[1]])[pval.melt$Var3]
  g3 <- ggplot(pval.melt) +
    geom_tile(aes(x = factor(Var1, levels = unique(Var1)), y = factor(Var2, levels = unique(Var2)), fill = value)) +
    geom_text(aes(x = factor(Var1, levels = unique(Var1)), y = factor(Var2, levels = unique(Var2)), label = ifelse(value <= .05, "*", NA)), color = "white") +
    scale_fill_gradient2(mid = c("dark red", "dark blue")[mod.type], high = "white", name = "pval") +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_wrap(vars(Var3), nrow = 2)
  
  ggsave(filename = paste0("~/Documents/CBBCoreTopics/Q2_", c("Physician", "Nurse")[mod.type], "_pval.png"), g3, width = 12, height = 4)
}


