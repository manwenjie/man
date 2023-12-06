oneComingStudentMajor <- sample(c("Economics", "Sociology"), 1)
# oneComingStudentMajor == "Economics"
print("Please go to room 201.")

# oneComingStudentMajor == "Sociology"
print("Please go to room 301.")
if(oneComingStudentMajor == "Economics"){
  print("Please go to room 201.")
}

if(oneComingStudentMajor == "Sociology"){
  print("Please go to room 301.")
}
if(...){....}
# T flag will always release the block for execution
if(T){print("TRUE block")}
# F flag will never release the block for execution
if(F){print("FALSE block")}
# T flag will always release the block for execution
if(c(T,T)){print("TT")}
if(c(T,F)){print("TF")}
if(c(F,F)){print("FF")}
if(c(F,T)){print("FT")}
# Use any() / all() if appropriate
if(c("a", "b")=="a"){print("case 1")}
if(any(c("a", "b")=="a")){print("case 2")}
if(all(c("a", "b")=="a")){print("case 3")}
# use `isTRUE` to ensure the block is relased only when a legitimate flag value of TRUE happens
if(isTRUE(T)){print("case 1")}
if(isTRUE("a")){print("case 2")}
if(isTRUE(c(T,T))){print("case 3")}
if(isTRUE(NA)){print("case 4")}

# there is also `isFALSE`.
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
rstudioapi::showPrompt('', message="Input r for rock, p for paper, or s for scissor") -> userChoice

computerChoice <- sample(c("r", "p", "s"), 1)

print(glue::glue("You chose {userChoice}, computer chose {computerChoice}"))

# If user won
print("You won.")

# If computer won,
print("Computer won.")

# If tie,
print("There is a tie.")
if(flag_userwin){print("You won.")}
if(flag_computerwin){print("Computer won.")}
if(flag_tie){print("There is a tie.")}
combination <- paste0(userChoice, computerChoice)

flag_userwin <- combination %in% c("rs", "sp", "pr")
flag_computerwin <- combination %in% c("sr", "ps", "pr")
flag_tie <- combination %in% c("rr", "ss", "pp")
rstudioapi::showPrompt('', message="Input r for rock, p for paper, or s for scissor") -> userChoice

computerChoice <- sample(c("r", "p", "s"), 1)

print(glue::glue("You chose {userChoice}, computer chose {computerChoice}"))

combination <- paste0(userChoice, computerChoice)

### three control flows
# r > s > p > r
if(combination %in% c("rs", "sp", "pr")){print("You won.")}
if(combination %in% c("sr", "ps", "rp")){print("Computer won.")}
if(combination %in% c("rr", "ss", "pp")){print("There is a tie.")}
get_rock_paper_scissor_combination <- function(){
  rstudioapi::showPrompt('', message="Input r for rock, p for paper, or s for scissor") -> userChoice
  
  computerChoice <- sample(c("r", "p", "s"), 1)
  
  print(glue::glue("You chose {userChoice}, computer chose {computerChoice}"))
  
  outcome <- c("user"=userChoice, "computer"=computerChoice)
  
  return(outcome)
}
outcome <- get_rock_paper_scissor_combination()
combination <- paste(outcome, collapse="")

### one control flow
# r > s > p > r
if(combination %in% c("rs", "sp", "pr")){print("You won.")} else
  if(combination %in% c("sr", "ps", "rp")){print("Computer won.")} else
    if(combination %in% c("rr", "ss", "pp")){print("There is a tie.")}
outcome <- get_rock_paper_scissor_combination()
combination <- paste(outcome, collapse="")
# r > s > p > r
if(combination %in% c("rs", "sp", "pr")){print("You won.")} else
  if(combination %in% c("sr", "ps", "rp")){print("Computer won.")} else
  {print("There is a tie.")}
outcome <- get_rock_paper_scissor_combination()
combination <- paste(outcome, collapse="")

flag_valid <- outcome[["user"]] %in% c("r", "s", "p")
# r > s > p > r
if(flag_valid & combination %in% c("rs", "sp", "pr")){print("You won.")} else
  if(flag_valid & combination %in% c("sr", "ps", "rp")){print("Computer won.")} else
    if(flag_valid){print("There is a tie.")} else
    {print("Please input only r, s or p.")}
outcome <- get_rock_paper_scissor_combination()
combination <- paste(outcome, collapse="")

flag_valid <- outcome[["user"]] %in% c("r", "s", "p")
# r > s > p > r
if(flag_valid && combination %in% c("rs", "sp", "pr")){print("You won.")} else
  if(flag_valid && combination %in% c("sr", "ps", "rp")){print("Computer won.")} else
    if(flag_valid){print("There is a tie.")} else
    {print("Please input only r, s or p.")}
TRUE || x # will return TRUE even x is unknown.
TRUE | x # will return ERROR
FALSE && x # will return FALSE even x is unknown.
FALSE & x # will return ERROR
if(flag1){block1} else
  if(flag2){block2} else
  {block3} 
if(flag1){block1} else
  if(!flag1 && flag2){block2} else
    if(!flag1 && !flag2){block3}
outcome <- get_rock_paper_scissor_combination()
combination <- paste(outcome, collapse="")

flag_valid <- outcome[["user"]] %in% c("r", "s", "p")
# r > s > p > r
if(!flag_valid){
  print("Please input only r, s or p.")
} else
  if(combination %in% c("rs", "sp", "pr")){print("You won.")} else
    if(combination %in% c("sr", "ps", "rp")){print("Computer won.")} else
    {print("There is a tie.")}
dataSet1 <- 
  data.frame(
    cat = c('a','a','b','a','c','a','b','a','b','a','b','a','a','b','a'),
    num=c(75,81,68,81,68,85,58,55,70,83,78,83,67,87,70))
# summarise a data vector, say data_vector

# (1) construct flags
flag_categorical <- is.character(data_vector) || is.factor(data_vector) || is.logical(data_vector)
flag_numerical <- is.numeric(data_vector)

# (2) Assemble control flow
if(flag_categorical){
  summarise_categorical(data_vector)
} else if(flag_numerical){
  summarise_numerical(data_vector)
}
{
  output_table = table(data_vector)
  output_mode = output_table[(output_table == max(output_table))]
  list(
    table=output_table,
    mode=output_mode
  )
}
{
  output_mean = mean(data_vector, na.rm=T)
  output_range = range(data_vector, na.rm=T)
  list(
    mean=output_mean,
    range=output_range
  )
}
# summarise a data vector, say data_vector

# (1) construct flags
flag_categorical <- is.character(data_vector) || is.factor(data_vector) || is.logical(data_vector)
flag_numerical <- is.numeric(data_vector)

# (2) Assemble control flow
if(flag_categorical){
  output_table = table(data_vector)
  output_mode = output_table[(output_table == max(output_table))]
  list(
    table=output_table,
    mode=output_mode
  )
} else if(flag_numerical){
  output_mean = mean(data_vector, na.rm=T)
  output_range = range(data_vector, na.rm=T)
  list(
    mean=output_mean,
    range=output_range
  )
}
summarise_categorical <- function(data_vector){
  output_table = table(data_vector)
  output_mode = output_table[(output_table == max(output_table))]
  list(
    table=output_table,
    mode=output_mode
  )
}

summarise_numerical <- function(data_vector){
  output_mean = mean(data_vector, na.rm=T)
  output_range = range(data_vector, na.rm=T)
  list(
    mean=output_mean,
    range=output_range
  )
}
# summarise a data vector, say data_vector
# (0) load summarise path function (can also put in a source .R script then source it first)
summarise_categorical <- function(data_vector){
  output_table = table(data_vector)
  output_mode = output_table[(output_table == max(output_table))]
  list(
    table=output_table,
    mode=output_mode
  )
}

summarise_numerical <- function(data_vector){
  output_mean = mean(data_vector, na.rm=T)
  output_range = range(data_vector, na.rm=T)
  list(
    mean=output_mean,
    range=output_range
  )
}

# (1) construct flags
flag_categorical <- is.character(data_vector) || is.factor(data_vector) || is.logical(data_vector)
flag_numerical <- is.numeric(data_vector)

# (2) Assemble control flow
if(flag_categorical){
  summarise_categorical(data_vector)
} else if(flag_numerical){
  summarise_numerical(data_vector)
}
# summarise a data vector, say data_vector
# (0) load summarise path function (can also put in a source .R script then source it first)
summarise_categorical <- function(data_vector){
  output_table = table(data_vector)
  output_mode = output_table[(output_table == max(output_table))]
  list(
    table=output_table,
    mode=output_mode
  )
}

summarise_numerical <- function(data_vector){
  output_mean = mean(data_vector, na.rm=T)
  output_range = range(data_vector, na.rm=T)
  list(
    mean=output_mean,
    range=output_range
  )
}

summarise_data_vector <- function(data_vector) {
  # (1) construct flags
  flag_categorical <- is.character(data_vector) || is.factor(data_vector) || is.logical(data_vector)
  flag_numerical <- is.numeric(data_vector)
  
  # (2) Assemble control flow
  if(flag_categorical){
    summarise_categorical(data_vector)
  } else if(flag_numerical){
    summarise_numerical(data_vector)
  }
}
summarise_data_vector(dataSet1$cat)
summarise_data_vector(dataSet1$num)
summarise_dataFrame <- function(dataSet1){
  .summary <- vector("list", length(dataSet1))
  for(.x in seq_along(dataSet1)){
    .summary[[.x]] <- summarise_data_vector(dataSet1[[.x]])
  }
  names(.summary) <- names(dataSet1) # element names as data set column names
  return(.summary)
}
summarise_dataFrame(dataSet1)
