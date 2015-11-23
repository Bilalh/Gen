DF <- data.frame(
  WindDir = sample(0:180, 20, replace = T),
  WindSpeed = sample(1:40, 20, replace = T),
  Force = sample(1:40, 20, replace = T)
)


ggplot(transform(DF,
                 fct = cut(WindDir, seq(0,180,3)) ),
       aes(WindSpeed, Force)) +
  geom_point() +
  facet_wrap( ~ fct)


mylist <- list('a'=10,'b'=20,'c'=30)
for (i in seq_along(mylist)){
    print(paste(i,names(mylist)[i],mylist[[i]]))
 }


authors <- data.frame(
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))
books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))
