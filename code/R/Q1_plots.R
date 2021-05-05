set.seed(1234)

# Read in the data:
df <- read.csv(file = "~/Downloads/Q1_SG.csv")
rownames(df) <- df$X
colnames(df)[7:9] <- c(":", ")", "(")
df$total <- sqrt(df$total)

df.melt = reshape2::melt(df, id.vars = "X")
df.melt$is.total = ifelse(df.melt$variable != "total", "Term", "sqrt(Total)")
df.melt$is.total = ifelse(df.melt$variable %in% c(":", ")", "("), "Symbol", df.melt$is.total)

df.melt$variable = factor(df.melt$variable, levels = c(":",
                                                       ")",
                                                       "(",
                                                       "hello",
                                                       "good",
                                                       "great",
                                                       "glad",
                                                       "thank",
                                                       "joy",
                                                       "enjoy",
                                                       "hope",
                                                       "sorri",
                                                       "frustrat",
                                                       "total"))

g1 <- ggplot2::ggplot(df.melt) +
  ggplot2::geom_point(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::geom_line(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Frequency of terms for all Providers (2012 - 2020)") +
  ggplot2::scale_color_manual(values = c("red", "blue", "green", "purple", "maroon", "orange",
                                         "dark red", "dark blue", "dark green", "light blue", "light green", "tomato",
                                         "grey", "black")) +
  ggplot2::facet_wrap(ggplot2::vars(is.total)) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Count") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))


# Plot the percentages:
# Read in the data:
df <- read.csv(file = "~/Downloads/Q1_SG.csv")
rownames(df) <- df$X
colnames(df)[7:9] <- c(":", ")", "(")
df[2:14] <- apply(df[2:14], 2, function(col){return(col/df$total)})
df <- df[1:14]

df.melt = reshape2::melt(df, id.vars = "X")
df.melt$is.total = ifelse(df.melt$variable != "total", "Term", "sqrt(Total)")
df.melt$is.total = ifelse(df.melt$variable %in% c(":", ")", "("), "Symbol", df.melt$is.total)

df.melt$variable = factor(df.melt$variable, levels = c(":",
                                                       ")",
                                                       "(",
                                                       "hello",
                                                       "good",
                                                       "great",
                                                       "glad",
                                                       "thank",
                                                       "joy",
                                                       "enjoy",
                                                       "hope",
                                                       "sorri",
                                                       "frustrat"))

g2 <- ggplot2::ggplot(df.melt) +
  ggplot2::geom_point(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::geom_line(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Frequency of terms for all Providers (2012 - 2020)") +
  ggplot2::scale_color_manual(values = c("red", "blue", "green", "purple", "maroon", "orange",
                                         "dark red", "dark blue", "dark green", "light blue", "light green", "tomato",
                                         "grey")) +
  ggplot2::facet_wrap(ggplot2::vars(is.total)) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Count/TotalCounts") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))

ggplot2::ggsave(filename = "~/Documents/CBBCoreTopics/Q1_fig.png", plot = g1, units = "in", height = 4, width = 8)
ggplot2::ggsave(filename = "~/Documents/CBBCoreTopics/Q1Norm_fig.png", plot = g2, units = "in", height = 4, width = 8)

