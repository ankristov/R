library(dplyr)
library(readr)
f <-  file(description = '/Users/Andrew/Desktop/Coursera/data/indiv14/itcont.txt')
lines <- readLines(con=f)
indivDict <- data.frame()
for (i in 1:length(lines)) {
  line_as_vector <- unlist(strsplit(lines[i],split = "|", fixed = T))
  indivDict <- rbind(indivDict, line_as_vector)
  if (i %% 100000 == 0) {print(i)}
}
strsplit(l,"\|")



indivDict <- read.table(file='/Users/Andrew/Desktop/Coursera/data/indiv14/itcont.txt', 
                        sep="|", as.is = T)
problems(indivDict)

indivDict[50,]
indivDict <- select(indivDict, V8, V15)
names(indivDict) <- c('company', "donation")
head(indivDict)
head(indivDict[order(indivDict$donation),])
indivDict_sorted <- arrange(indivDict, donation)
head(indivDict_sorted,5)





# Make plot
Data = read.table('/Users/Andrew/Desktop/Coursera/data/employerMoney.txt' ,sep=';', as.is = TRUE)
colnames(Data) = c('Company', 'Rep', 'Dem', 'Other', 'Total')
head(Data)
print(Data[,1])
s = 160:190  # Select specific rows to plot.
D = Data[s,]                # Take the subset.
D = D[order(D$Rep+D$Dem),]  # Re-order the data according to the total.
rep = D$Rep/10^5            # Scale the values.
dem = D$Dem/10^5
mx = max(rep+dem)
names = D[,1]
n = length(rep)
# Fix the plot window for long names.
plot(x = c(0,mx),y=c(1,n),yaxt = 'n',xlab
     = "Dollars - 100,000's",cex.axis = .65,typ = 'n',ylab='',cex.lab=.8)
axis(side = 2, at = seq(1,n),labels = names, las = 2, cex.axis = .65)
for (i in 1:n) {
  lines(y=c(i,i),x=c(0,rep[i]),col='red',lwd=3)
  lines(y=c(i,i),x=c(rep[i],rep[i]+dem[i]),col='blue',lwd=3)
}
par(oma=c(0,0,0,0))  # Reset the plotting window to default values.
