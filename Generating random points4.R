install.packages("spatstat")   ##Install the spatstat package
library(spatstat)             ##Load the spatstat package   

set.seed(1)
ppp_list<-list()   #generate a empty list to store ppp objects
p<-90           #set p to the number of random patterns generated
df<-data.frame(Pattern_no=seq(1:p))   #generate a empty dataframe to store information on pattern no, and number of dots


#create a function that generate our presentation stimuli
plot_generator<-function(ppp_object){
  #plot the pattern, remove title, and against a blue background window 
  plot.ppp(ppp_object, main=NULL, show.window = TRUE, col="blue")
  #customise window size so that the window does not cut any of the points
  B <- owin(c(-0.025, 1.025), c(-0.025, 1.025))
  #superimpose the customise window over plot.ppp
  plot(B,add=TRUE,edge=0.06,show.window=TRUE, col="Blue")   
  #enlarge points and use fill color to yellow
  points(ppp_object,cex=2,pch=19,col="yellow")
  #print out the corresponding dots for verification purposes
  print(ppp_object$n)
}


#Option 1: generate for loop to generate 90 different random dots patterns  
for(i in 1: p){          
  ppp_list[[i]]<-rSSI(r=0.045,n=sample(30:80, 1))  ##Generate random point pattern (SSI), with random number of points between 30 to 80
  df[i,"number_dots"]<-ppp_list[[i]]$n     #naming the column and placing the number of dots into dataframe
  plot_generator(ppp_list[[i]])           #plotting each of the ppp objects using our function   
}

names(ppp_list)<-seq(1:p)   #naming each of the list within ppp_list; meant for the title of the plot

#Option 2: a second for loop to generate the plots with title (need to run for loop options 1 prior to running option 2)
for(i in 1: p){          
  ppp_list[[i]]<-rSSI(r=0.045,n=sample(30:80, 1))  ##Generate random point pattern (SSI), with random number of points between 30 to 80
  df[i,"number_dots"]<-ppp_list[[i]]$n     #naming the column and placing the number of dots into dataframe
  plot_generator(ppp_list[[i]])
  title(main = paste("Pattern","", names(ppp_list[i]))) #giving a title for each plot; title as the number of patterns
}


#Option 3: a third for loop to save the respective dots patterns in high resolution (need to run for loop options 1 prior to running option 3)
for(i in 1: p){          
  ppp_list[[i]]<-rSSI(r=0.045,n=sample(30:80, 1))  ##Generate random point pattern (SSI), with random number of points between 30 to 80
  df[i,"number_dots"]<-ppp_list[[i]]$n     #naming the column and placing the number of dots into dataframe
  name_file <- paste("Image_", names(ppp_list[i]),".jpeg")     #creating the file name for our patterns
  jpeg(name_file,width=6,height=6,units="in",res=600)         #creating the file for our patterns   
  plot_generator(ppp_list[[i]])
  title(main = paste("Pattern","", names(ppp_list[i]))) #giving a title for each plot; title as the number of patterns
  dev.off()   #to save jpeg file in device
}

##For data frame; Only run this code after the desired for loop 
##(Note. Each of the three for loops returns different results and their respective dataframe should not be assume to be the same).
View(df)
library("writexl")
write_xlsx(df,"C:\\Users\\daxng\\Dropbox\\Desktop\\No_of_Dots.xlsx")
