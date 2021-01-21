library(methods)


# Class "animal" ----------------------------------------------------------

# random pronouncable strings with length <length>
make_name <- function(length = 7L) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1L] <- sample(toupper(consonants), 1L)
  name[seq(3L, length, by = 2L)] <-
    sample(consonants, size = ceiling(length / 2L) - 1L, replace = TRUE)
  name[seq(2L, length, by = 2L)] <-
    sample(vowels, size = floor(length / 2L), replace = TRUE
    )
  paste(name, collapse = "")
}

# random sex, 'male' or 'female' possible (logical wrt female)
make_sex <- function() {
  sample(c(FALSE, TRUE), size = 1L)
}

setClass("animal", slots = c(name = "character", weight = "numeric",
                             female = "logical"),
         prototype = list(name = "Buddy", weight = 1, female = TRUE)
)

# Helper
animal <- function(name, weight, female) {
  new("animal", name = name, weight = weight, female = female)
}

# Validator
setValidity("animal", function(object) {
  invalids <- character(0)
  no_name <- nchar(object@name) == 0
  wrong_weight <- (object@weight <= 0 || object@weight == Inf || is.na(object@weight))
  wrong_sex <- typeof(object@female) != "logical"
  if (no_name) {
    invalids <- "Animals need a <name>"
  }
  if (wrong_weight) {
    invalids <- c(invalids, "<weight> should be a positive finite value")
  }
  if (wrong_sex) {
    invalids <- c(invalids, "<female> should be a boolean (TRUE/FALSE)")
  }
  if (length(invalids)) {
    invalids
  }
  else {
    TRUE
  }
})



# Help functions for subclass validators ----------------------------------

invalids_prey <- function(object, weight_min, weight_max, hide_min, hide_max) {
  invalids <- character(0)
  wrong_weight <- (object@weight < weight_min || object@weight > weight_max)
  wrong_hide <- (object@hide < hide_min || object@hide > hide_max)
  if (wrong_weight) {
    invalids <- paste("<weight> should be in [", weight_min, ", ", weight_max, "]", 
                    sep = "")
  }
  if (wrong_hide) {
    invalids <- c(invalids, paste("<hide> should be in [", hide_min, ",", hide_max, "]", 
                    sep = ""))
  }
  if (length(invalids)) {
    invalids
  }
  else {
    TRUE
  }
}


invalids_predator <- function(object, weight_min, weight_max, seek_min, seek_max) {
  invalids <- character(0)
  wrong_weight <- (object@weight < weight_min || object@weight > weight_max)
  wrong_seek <- (object@seek < seek_min || object@seek > seek_max)
  if (wrong_weight) {
    invalids <- paste("<weight> should be in [", weight_min, ",", weight_max, "]", 
                    sep = "")
  }
  if (wrong_seek) {
    invalids <- c(invalids, paste("<seek> should be in [", seek_min, ",", seek_max, "]", 
                    sep = ""))
  }
  if (length(invalids)) {
    invalids
  }
  else {
    TRUE
  }
}


# Subclass "prey" ---------------------------------------------------------

# additional slot: <hide>
setClass("prey",
         contains = "animal",
         slots = c(hide = "numeric")
)


# Subsubclass "mouse"

setClass("mouse",
         contains = "prey")

mouse <- function(name = make_name(), weight = runif(1L, min = 0.5, max = 1),
                  female = make_sex(), hide = runif(1L, min = 0.6, max = 1)) {
  new("mouse", name = name, weight = weight, female = female, hide = hide)
}

setValidity("mouse", function(object) {
  invalids_prey(object, 
                weight_min = 0.5, weight_max = 1, 
                hide_min = 0.6, hide_max = 1)
})


# Subsubclass "rabbit"
setClass("rabbit",
         contains = "prey"
)

rabbit <- function(name = make_name(), weight = runif(1L, min = 1, max = 5),
                   female = make_sex(), hide = runif(1L, min = 0.3, max = 0.8)) {
  new("rabbit", name = name, weight = weight, female = female, hide = hide)
}

setValidity("rabbit", function(object) {
  invalids_prey(object,
                weight_min = 1, weight_max = 5, 
                hide_min = 0.3, hide_max = 0.8)
})

# Subsubclass "deer"
setClass("deer",
         contains = "prey"
)

deer <- function(name = make_name(), weight = runif(1L, min = 15, max = 30),
                 female = make_sex(), hide = runif(1L, min = 0.2, max = 0.7)) {
  new("deer", name = name, weight = weight, female = female, hide = hide)
}

setValidity("deer", function(object) {
  invalids_prey(object, 
                weight_min = 15, weight_max = 30, 
                  hide_min = 0.2, hide_max = 0.7)
})



# Subclass "predator" -----------------------------------------------------

setClass("predator",
         contains = "animal",
         slots = c(seek = "numeric")
)


# Subsubclass "hawk"
setClass("hawk",
         contains = "predator"
)

hawk <- function(name = make_name(), weight = runif(1L, min = 3, max = 8),
                 female = make_sex(), seek = runif(1L, min = 0.6, max = 1)) {
  new("hawk", name = name, weight = weight, female = female, seek = seek)
}

setValidity("hawk", function(object) {
  invalids_predator(object,
                    weight_min = 3, weight_max = 8, 
                    seek_min = 0.6, seek_max = 1)
})

# Subsubclass "lynx"
setClass("lynx",
         contains = "predator"
)

lynx <- function(name = make_name(), weight = runif(1L, min = 20, max = 60),
                 female = make_sex(), seek =  runif(1L, min = 0.5, max = 0.9)) {
  new("lynx", name = name, weight = weight, female = female, seek = seek)
}

setValidity("lynx", function(object) {
  invalids_predator(object, 
                    weight_min = 20, weight_max = 60, 
                    seek_min = 0.5, seek_max = 9)
})



# Method "show" -----------------------------------------------------------

setMethod("show", signature("animal"), function(object) {
  cat("## ", is(object)[[1]], " '", object@name, "'", sep = "")
  cat(ifelse(object@female, " (f)", " (m)"), "\n", sep = "")
  cat("##   weight: ", object@weight, "\n", sep = "")
})

setMethod("show", signature("prey"), function(object) {
  callNextMethod()
  cat("##   hide: ", object@hide, "\n", sep = "")
})

setMethod("show", signature("predator"), function(object) {
  callNextMethod()
  cat("##   seek: ", object@seek, "\n", sep = "")
})

# Methods "meet" ----------------------------------------------------------
# Sex doesn't matter for "love", just species

setGeneric("meet", function(animal1, animal2, ...) {
  standardGeneric("meet")
})


# produce a "meet_message"
meet_message <- function(animal1, animal2, action) {
  animal1_message <- cat("## ", class(animal1), " '", animal1@name, "'", sep = "")
  animal2_message <- cat(class(animal2), " '", animal2@name, "'", sep = "")
  
  switch(action,
         "ignore" = return(cat(animal1_message, "&", 
                               animal2_message, "ignore each other", "\n", sep = " ")),
         "love" = return(cat(animal1_message, "&", 
                             animal2_message, "make sweet, sweet love", "\n", sep = " ")),
         "sniff" = return(cat(animal1_message, "&", 
                              animal2_message, "sniff each others' butts", "\n", sep = " ")),
         "fight" = return(cat(animal1_message, "&", 
                              animal2_message, "fight for territory", "\n", sep = " ")),
         "kill" = return(cat(animal1_message, "kills and eats", 
                             animal2_message, "\n", sep = " ")),
         "escape" = return(cat(animal1_message, "escapes from", 
                               animal2_message, "\n", sep = " "))
  )
  if (identical(animal1, animal2)) {
  return(cat(animal1_str, "gazes at her reflection in a puddle", "\n", sep = " "))
  }
}


# prey - prey
setMethod("meet", signature("prey", "prey"), function(animal1, animal2) {
  same_species <- class(animal1) == class(animal2)
  if (same_species) {
    action <- sample(c("ignore", "sniff", "love"), 1, prob = c(0.25, 0.25, 0.5))
  } else {
    action <- sample(c("ignore", "sniff"), 1, prob = c(0.5, 0.5))
  }
  meet_message(animal1, animal2, action = action)
})

# predator - predator
setMethod("meet", signature("predator", "predator"), function(animal1, animal2) {
  same_species <- class(animal1) == class(animal2)
  if (same_species) {
    action <- sample(c("fight", "love"), 1, prob = c(0.5, 0.5))
  } else {
    action <- sample(c("ignore", "sniff", "fight"), 1, prob = c(1/3, 1/3, 1/3))
  }
  meet_message(animal1, animal2, action = action)
})

# predator - prey & prey - predator
setMethod("meet", signature("predator", "prey"), function(animal1, animal2) {
  predator <- animal1
  prey <- animal2
  small_prey <- prey@weight >= 0.05 * predator@weight && prey@weight <= 0.7 * predator@weight
  
  if (small_prey) {
    kill_prob <- min(1, max(0, 0.6 + predator@seek - prey@hide))
    action <- sample(c("kill", "escape"), 1, prob = c(kill_prob, 1 - kill_prob))
  } else {
    action <- sample(c("ignore", "sniff"), 1, prob = c(0.5, 0.5))
  }
  
  meet_message(predator, prey, action = action)
})

# just switch the roles of prey and predator
setMethod("meet", signature("prey", "predator"), function(animal1, animal2) {
  callGeneric(animal2, animal1)
})




