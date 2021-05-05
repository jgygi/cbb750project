set.seed(1234)

# Read in the data:
df <- read.csv(file = "~/Downloads/Q2_Physician.csv")
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

df.melt$Prov = "Physician"

df.melt.phys = df.melt


# Read in the data:
df <- read.csv(file = "~/Downloads/Q2_Nurse.csv")
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

df.melt$Prov = "Nurse Practitioner"

df.melt.nurse = df.melt

df.melt.comb = rbind(df.melt.phys, df.melt.nurse)

g1 <- ggplot2::ggplot(df.melt.comb) +
  ggplot2::geom_point(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::geom_line(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Frequency of terms for Physicians vs Nurse Practitioners (2012 - 2020)") +
  ggplot2::scale_color_manual(values = c("red", "blue", "green", "purple", "maroon", "orange",
                                         "dark red", "dark blue", "dark green", "light blue", "light green", "tomato",
                                         "grey", "black")) +
  ggplot2::facet_grid(rows = ggplot2::vars(is.total), cols = ggplot2::vars(Prov)) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Count") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))






# Read in the data:
df <- read.csv(file = "~/Downloads/Q2_Physician.csv")
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
                                                       "frustrat",
                                                       "total"))

df.melt$Prov = "Physician"

df.melt.phys = df.melt


# Read in the data:
df <- read.csv(file = "~/Downloads/Q2_Nurse.csv")
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
                                                       "frustrat",
                                                       "total"))

df.melt$Prov = "Nurse Practitioner"

df.melt.nurse = df.melt

df.melt.comb = rbind(df.melt.phys, df.melt.nurse)

g2 <- ggplot2::ggplot(df.melt.comb) +
  ggplot2::geom_point(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::geom_line(ggplot2::aes(x = X, y = value, group = variable, color = variable)) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("Frequency of terms for Physicians vs Nurse Practitioners (2012 - 2020)") +
  ggplot2::scale_color_manual(values = c("red", "blue", "green", "purple", "maroon", "orange",
                                         "dark red", "dark blue", "dark green", "light blue", "light green", "tomato",
                                         "grey", "black")) +
  ggplot2::facet_grid(rows = ggplot2::vars(is.total), cols = ggplot2::vars(Prov)) +
  ggplot2::xlab("Year") +
  ggplot2::ylab("Count/TotalCounts") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))

g2

ggplot2::ggsave(filename = "~/Documents/CBBCoreTopics/Q2_PhysvsNurse_fig.png", plot = g1, units = "in", height = 8, width = 8)
ggplot2::ggsave(filename = "~/Documents/CBBCoreTopics/Q2Norm_PhysvsNurse_fig.png", plot = g2, units = "in", height = 6, width = 8)
