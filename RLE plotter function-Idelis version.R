# make sure that you set up the right work directory
library(NanoStringNorm)
library(openxlsx)
rm(list=ls())

talita <- read.xlsx("RLE_data.xlsx")
t_nanos <- NanoStringNorm(talita)

code_count <- c('none', 'sum', 'geo.mean', 'none', 'none', 'none')
back_ground <- c('none', 'mean', "mean.2sd", "max", 'none', 'none')
sample_content <- c('none', 'housekeeping.sum', "housekeeping.geo.mean" , 'total.sum', "low.cv.geo.mean", 'top.mean', 'top.geo.mean')


for (i in code_count) {
  for(j in back_ground){
for(k in sample_content)    {
  t_nanos <- NanoStringNorm(talita, CodeCount = i, Background = j, SampleContent = k)
  t_log2 <-t_log2 <- log2(t_nanos$normalized.data[,4:87])
  t_means <- apply(t_log2, 1, median)
  test <- t_log2 - t_means
  
pdf(paste("code count", i, "background",j,"sample_content", k, ".pdf"))
  par(mar = c(7, 6.0, 2, 2), mgp = c(4.5, 1, 0))
boxplot(test, las = 2, cex.axis = 0.5,  ylab = "Relative Log Expression", main = paste(i, j, k))
abline(h = 0, col = "red")
dev.off()

}

}
}

library(qpdf)


name<-list.files(pattern = "\\.pdf$")
qpdf::pdf_combine(input = c(name),
                  output = "output.pdf")

for (i in code_count) {
  for(j in back_ground){
    for(k in sample_content)    {
      t_nanos <- NanoStringNorm(talita, CodeCount = i, Background = j, SampleContent = k)
      t_log2 <-t_log2 <- log2(t_nanos$normalized.data[,4:87])
      
      write.csv(t_nanos$normalized.data, file = paste("code count", i, "background",j,"sample_content", k, ".csv"))
    }
    
  }
}
View(head(t_nanos))

t_log2 <- log2(t_nanos$normalized.data[,4:87])
t_means <- apply(t_log2, 1, median)
test <- t_log2 - t_means
rownames(t_log2[1:length(t_log2)])
View(head(test))
View(head(t_log2))
View(head(t_means))

t_log2$X20190412_Talita.NeuroPath.ON273480_2120_01[1:10]
t_means[1:10]
test$X20190412_Talita.NeuroPath.ON273480_2120_01[1:10]



par(mar = c(7, 6.0, 2, 2), mgp = c(4.5, 1, 0))
boxplot(test, las = 2, cex.axis = 0.5,  ylab = "Relative Log Expression", main = "RLE Plot")
abline(h = 0, col = "red")


