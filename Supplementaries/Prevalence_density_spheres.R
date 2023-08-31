library(readxl)

pdf(file = "Prevalence_density.pdf",   # The directory you want to save the file in
    width = 5.5, # The width of the plot in inches
    height = 6) # The height of the plot in inches

# Step 2: Create the plot with R code
hist_data<-read_excel("/Users/arijitmukherjee/Downloads/histogram.xlsx",sheet="Sheet1",col_names = T,skip = 0)
hist_data
dx<-density(hist_data$Soil)
dy<-density(hist_data$Rhizosphere)
dz<-density(hist_data$Phyllosphere)

plot(dz, lwd = 2, col = "red",
     main = "", xlab = "")
lines(dy, col = "blue", lwd = 2)
lines(dx,col="black",lwd=2)
#Phylosphere red, Rhizosphere blue, Soil black
# Step 3: Run dev.off() to create the file!
dev.off()



