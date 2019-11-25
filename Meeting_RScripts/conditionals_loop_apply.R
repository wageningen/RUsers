## Lecture on conditionals, functoins, apply and foreach
## Dainius Masiliunas, 2019
## CC0 license
## Partly based on functions_loop_apply.R

## Datasets

str(iris)
head(iris)

halfiris = iris[3:length(iris)]

head(faithful)

## Conditionals

# Function for setting some columns to integer and some to character
MakeColumnsInteger_wrong = function(data) {
    data$Sepal.Length = as.integer(data$Sepal.Length)
    data$Sepal.Width = as.integer(data$Sepal.Width)
    data$Petal.Length = as.integer(data$Petal.Length)
    data$Petal.Width = as.integer(data$Petal.Width)
    data$Species = as.character(data$Species)
    return(data)
}

str(MakeColumnsInteger_wrong(iris))
str(MakeColumnsInteger_wrong(faithful)) # Error, no Sepal.Length in faithful
str(MakeColumnsInteger_wrong(halfiris)) # Error as well

# Use if to check if they exist
MakeColumnsInteger_if = function(data) {
    ColNames = colnames(data)
    if (any(ColNames == "Sepal.Length")) {
        data$Sepal.Length = as.integer(data$Sepal.Length)
    }
    if (any(ColNames == "Sepal.Width")) {
        data$Sepal.Width = as.integer(data$Sepal.Width)
    }
    if (any(ColNames == "Petal.Length")) {
        data$Petal.Length = as.integer(data$Petal.Length)
    }
    if (any(ColNames == "Petal.Width")) {
        data$Petal.Width = as.integer(data$Petal.Width)
    }
    if (any(ColNames == "Species")) {
        data$Species = as.character(data$Species)
    }
    return(data)
}

# It all works
str(MakeColumnsInteger_if(iris))
str(MakeColumnsInteger_if(halfiris))
str(MakeColumnsInteger_if(faithful))

# Switch can make a lot of ifs more concise/readable
MakeColumnsInteger_switch = function(data) {
    ColNames = colnames(data)
    
    # Go through all the columns
    for (ColName in ColNames) {
        data[ColName] = switch(ColName, # Test what ColName is from a list of options
            "Sepal.Length" = as.integer(data$Sepal.Length),
            "Sepal.Width"  = as.integer(data$Sepal.Width),
            "Petal.Length" = as.integer(data$Petal.Length),
            "Petal.Width"  = as.integer(data$Petal.Width),
            "Species"      = as.character(data$Species),
            data[ColName] # Default: if none of the above
            )
    }
    return(data)
}

str(MakeColumnsInteger_switch(iris))
str(MakeColumnsInteger_switch(halfiris))
str(MakeColumnsInteger_switch(faithful))

# Same with ifelse
MakeColumnsInteger_ifelse = function(data) {
    ColNames = colnames(data)
    
    # Go through all the columns
    for (ColName in ColNames) {
        # ifelse is an "assignable if"
        data[ColName] = ifelse(ColName=="Sepal.Length", as.integer(data$Sepal.Length),
            ifelse(ColName=="Sepal.Width", as.integer(data$Sepal.Width),
                ifelse(ColName=="Petal.Length", as.integer(data$Petal.Length),
                    ifelse(ColName=="Petal.Width", as.integer(data$Petal.Width),
                        ifelse(ColName=="Species", as.character(data$Species),
                            data[ColName])))))
    }
    return(data)
}

str(MakeColumnsInteger_ifelse(iris))
str(MakeColumnsInteger_ifelse(halfiris))
str(MakeColumnsInteger_ifelse(faithful))

## Loops

# for loop that constructs a new data.frame
MakeColumnsInteger_for = function(data) {
    # Make a placeholder first; note: this is slow due to dynamic allocation
    Result = NULL
    for (Column in data) {
        if (is.numeric(Column)) {
            Result = as.data.frame(cbind(Result, as.integer(Column)))
        } else if (is.factor(Column)) {
            Result = as.data.frame(cbind(Result, as.character(Column), stringsAsFactors=FALSE))
        } else Result = as.data.frame(cbind(Result, Column))
    }
    names(Result) = names(data)
    return(Result)
}

str(MakeColumnsInteger_for(iris))
str(MakeColumnsInteger_for(halfiris))
str(MakeColumnsInteger_for(faithful))

# apply - run a function on each column or row
MakeColumnsInteger_apply = function(data) {
    ColNames = colnames(data)
    data[]apply()
    for (Column in data) {
        if (is.numeric(Column)) {
            Result = as.data.frame(cbind(Result, as.integer(Column)))
        } else if (is.factor(Column)) {
            Result = as.data.frame(cbind(Result, as.character(Column), stringsAsFactors=FALSE))
        } else Result = as.data.frame(cbind(Result, Column))
    }
    names(Result) = names(data)
    return(Result)
}

MakeColumnInteger = function(col)
{
    if (is.numeric(col))
        return(as.integer(col))
    if (is.factor(col)) # No need for else - return() makes sure there will never be an "else"
        return(as.character(col))
    return(col)
}
str(sapply(iris, MakeColumnInteger))

MakeColumnsInteger_apply = function(data) {
    # Make a placeholder first
    Result = NULL
    for (Column in data) {
        if (is.numeric(Column)) {
            Result = as.data.frame(cbind(Result, as.integer(Column)))
        } else if (is.factor(Column)) {
            Result = as.data.frame(cbind(Result, as.character(Column), stringsAsFactors=FALSE))
        } else Result = as.data.frame(cbind(Result, Column))
    }
    names(Result) = names(data)
    return(Result)
}
