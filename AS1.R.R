##### Statistische Software (R)
##### First graded Assignment
##### Winter Semester 22/23
##### 31.10.2022
##### César Andrés Pérez Castro
##### 12649783
  
heights <- c(
    "Anna" = 165.3,
    "Philipp" = 170.1,
    "Ludwig" = 181.4,
    "Carina" = 155.0,
    "Lisa" = 172.5,
    "Andrea" = 167.9,
    "Karim" = 179.1,
    "Daniel" = 187.3,
    "Judith" = 181.4,
    "Regina" = 159.8,
    "Julia" = 170.4,
    "Sven" = 176.0,
    "Hedwig" = 171.1,
    "Bernd" = 180.2)
  
heights
length(heights)
###################################### a)
  
typeof(heights) #The data type is double
  
###################################### b) 
  
length(heights) #The length of the vector is 14
  
###################################### c) 
  
heights2<- round(heights)
heights2
  
###################################### d) 
  
M<- c("Matthias"=  167.1) 
heights<- c(heights[1],M,heights[2:14])
heights
length(heights)
  
#I created the variable M an then inserted it into heights
  
###################################### e) 
  
#first way
  
heights[10]
  
#second way
heights["Judith"]
  
#I would use the second method because it does not require knowing the 
#position of "Judith" in the vector
  
###################################### f)
  
heights_meters<- heights/100
heights_meters

#to be able to do an operation between two vectors in R, both vectors need to be
#the same size. But R can reuse the values of a vector as long as it is a 
#multiple of the largest vector. In other words, if there is a vector of 20 
#elements, and one of 2 elements, R would repeat the 2-element vector 10 times 
#to complete the remaining elements. In this case, dividing by 100. What's 
#happening is that each element of the "heights" vector is being divided by 100.
#This is useful because it keeps the code shorter and more efficient. 
  
###################################### g)
below_av<- c(heights<=173.3)
below_av
  
###################################### h)
below_av_f<- factor(below_av,levels= c(TRUE, FALSE),labels=c("below average", 
                                                             "above average"))
below_av_f
table(below_av_f)
  
###################################### i)
  
#Group 1: ”Anna”, ”Carina”, ”Lisa”, ”Andrea”, ”Judith”, ”Regina”, ”Julia”
group1<-c(heights["Anna"],heights["Carina"],heights["Lisa"],heights["Andrea"],
            heights["Judith"],heights["Regina"],heights["Julia"])
group1
  
#Group 2: ”Matthias”, "Philipp”, ”Ludwig”, ”Karim”, ”Daniel”, ”Sven”, ”Hedwig”,
#"Bernd"
  
group2<-c(heights["Matthias"],heights["Philipp"],heights["Ludwig"],
          heights["Karim"],heights["Daniel"],heights["Sven"],heights["Hedwig"],
          heights["Bernd"])
group2
  
av_group1<- mean(group1)
av_group1
av_group2<- mean(group2)
av_group2
  
dif_g1_g2<- av_group2-av_group1
dif_g1_g2

#here I created 2 groups, and then calculated the average of each group and 
#then subtracted the results 

###################################### j)

heights_Final<- c(heights)+c(rep(c(0.1,-0.1),7),0.1)
heights_Final

#Here I used the rep function so that (0.1,-0.1) was repeated 7 times and since
#"heights" has a length of 15, I added one "0.1" more. This was necessary 
#because 15 is not a multiple of 2.  
