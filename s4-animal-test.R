# "show" method -----------------------------------------------------------

deer()
hawk()


invalids_prey(deer(), 120, 140, 3, 4)
invalids_prey(deer(), 15, 30, 0.2, 0.7)


# general structure -------------------------------------------------------

str(mouse(female = TRUE))
str(hawk(weight = 4))


# invalid structure ------------------------------------------------------

str(lynx(name = "", weight = NA + 1))
str(mouse(weight = 100))
str(mouse(weight = 100, hide = 0.01))

# "meet" method -----------------------------------------------------------

# example code for animal class: (results may vary.)
set.seed(20191121)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}
