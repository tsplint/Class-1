#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)


#reorder grade to increasing order
yrbss$Grade <- factor(
  yrbss$Grade,
  levels = c("9", "10", "11", "12", "other")
)

#check
table(yrbss$Grade)

### change capital letters in category labels

yrbss <- yrbss %>% 
  mutate(Gender = fct_relabel(Gender, str_to_title))
print(levels(yrbss$Gender))

#change all coloumn names
names(yrbss) <- stringr::str_to_title(names(yrbss))

# category lables
yrbss <- yrbss %>%
  mutate(across(where(is.character), ~ stringr::str_to_sentence(.)))

## summary table
z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
aggregate(Physically_active_7d ~ Grade + Gender, data=yrbss, FUN=mean)

f <- aggregate(Physically_active_7d ~ Grade + Gender, data=yrbss, FUN=mean)
table_activity_gender_by_grade <- as_flextable(f)
table_activity_gender_by_grade


# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

##Plot average physically active days by grade and gender
activity_grade_plot <- aggregate(Physically_active_7d ~ Grade + Gender, data=yrbss, FUN=mean, na.rm=TRUE) |>
  ggplot(aes(x=Grade, y= Physically_active_7d, color=Gender, group=Gender)) + 
  geom_line()+ labs(title = "Physical Activity by Grade",x="Grade", y="Average Physical Activity")

activity_grade_plot

# Question 3
# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

#create BMI label
data <- yrbss
data$BMI <-  data$Weight / (data$Height)^2

#plot BMI by activity 

BMI_plot <- data |>
  subset(Gender == "Female" & Grade == "12") |>
  ggplot(aes(x = BMI, y = Physically_active_7d)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Physical Activity and BMI Among Grade 12 Female Students",
    x = "BMI",
    y = "# of Physically Active days"
  )
BMI_plot
# Push your completed code to your GitHub repository
