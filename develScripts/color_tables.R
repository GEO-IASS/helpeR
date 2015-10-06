# ------------------------------------------------------------------------------
# YRD (from Ariel)
cols <- read.csv("D:/Repos/py_SVN/uc_bm/plots/bar_change/example/colors.csv")
classes <- read.csv("D:/Repos/py_SVN/uc_bm/plots/bar_change/example/LUT.csv")

df <- cols
df <- cbind(df, classes[3:18, "Classes"])
df <- df[, c(1,3,4,5,6,2, 7)]
colnames(df) <- c("Id", "R", "G", "B", "HEX", "Code", "Class", "y")
write.csv(df, file="F:/VIS/classcolors_YRD.csv")

df <- read.csv("F:/VIS/classcolors_YRD.csv")

require(ggplot2)
df$y <-1
str(df)

ggplot(df, aes(x = Class, y = y)) + 
  geom_bar(stat = "identity", fill=as.character(df$HEX)) + coord_flip()
