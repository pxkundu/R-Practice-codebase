# Load necessary libraries
install.packages("forecast", dep= TRUE)
library(forecast)

# Update this line of code with the dataset location
setwd("/Users/parthasarathikundu/Downloads")

# Load the dataset
gas_prod_input <- as.data.frame(read.csv("gas_prod.csv"))

gas_prod <- ts(gas_prod_input[,2])

plot(gas_prod, xlab = "Time (months)",
     ylab = "Gasoline production (millions of barrels")

plot(diff(gas_prod))

abline(a=0, b=0)

acf(diff(gas_prod), xaxp = c(0, 48, 4), lag.max=48, main="")

pacf(diff(gas_prod), xaxp = c(0,48,4), lag.max=48, main="")


arima_1 <- arima(gas_prod, order=c(0,1,0), seasonal = list(order=c(1,0,0), period=12))

arima_1

arima_2 <- auto.arima(gas_prod, ic='aic', trace = TRUE)

acf(arima_1$residuals, xaxp = c(0,48,4), lag.max=48, main="")

pacf(arima_1$residuals, xaxp = c(0,48,4), lag.max=48, main="")

arima_3 <- arima(gas_prod, order=c(0,1,1), seasonal = list(order=c(1,0,0), period=12))

arima_3

acf(arima_3$residuals, xaxp = c(0,48,4), lag.max=48, main="")

pacf(arima_3$residuals, xaxp = c(0,48,4), lag.max=48, main="")


# Create a data frame from the CSV data
data <- data.frame(
  Name = c("Anna Müller", "Maria Schmidt", "Lisa Nowak", "Eva Fischer", "Sarah Meier"),
  Email = c("[email address removed]", "[email address removed]", "[email address removed]", "[email address removed]", "[email address removed]"),
  Phone = c("+49 176 1234567", "+49 151 1234567", "+49 160 1234567", "+49 172 1234567", "+49 157 1234567"),
  Date_of_Birth = c("1998-05-12", "1999-02-21", "2000-09-08", "1997-11-15", "2001-03-29"),
  Nationality = c("German", "German", "German", "German", "German"),
  Address = c("Berlin, Germany", "Munich, Germany", "Heidelberg, Germany", "Karlsruhe, Germany", "Freiburg, Germany"),
  University_Institution = c("Humboldt University of Berlin", "Technical University of Munich", "University of Heidelberg", "Karlsruhe Institute of Technology", "University of Freiburg"),
  Degree_Program = c("Computer Science", "Electrical Engineering", "Mathematics", "Physics", "Biology"),
  Year_of_Study = c(5, 4, 3, 6, 2),
  Major_Specialization = c("Artificial Intelligence", "Robotics", "Data Science", "Quantum Computing", "Bioinformatics"),
  GPA = c(3.8, 3.5, 3.9, 3.7, 3.6),
  Programming_Languages = c("Python, Java, C++", "Python, C++", "Python, R", "Python, C++", "Python, R"),
  Data_Science_Tools = c("Python, R, SQL", "Python, R", "Python, R, SQL", "Python", "Python, R, SQL"),
  Machine_Learning_Frameworks = c("TensorFlow, PyTorch", "TensorFlow", "Scikit-learn", "TensorFlow Quantum", "Scikit-learn"),
  Cloud_Platforms = c("AWS, Azure, GCP", "AWS", "AWS", "AWS", "AWS"),
  Other_Technical_Skills = c("Cybersecurity, DevOps", "Embedded Systems", "Data Visualization", "Quantum Algorithms", "Bioinformatic tools"),
  Internships = c("Internship at Siemens", "Internship at Bosch", "Internship at SAP", "Internship at IBM Research", "No internships"),
  Projects = c("3 personal projects on GitHub", "2 group projects", "1 personal project on Kaggle", "1 group project", "1 personal project on GitHub"),
  Research_Papers = c("1 published research paper", "0 published research papers", "0 published research papers", "2 published research papers", "0 published research papers"),
  Open_Source_Contributions = c("Contributed to 2 open-source projects", "No open-source contributions", "No open-source contributions", "Contributed to 1 open-source project", "No open-source contributions")
)

# Install the required package
install.packages("writexl")

# Load the package
library(writexl)

# Write the data to an Excel file
write_xlsx(data, "student_data.xlsx")



