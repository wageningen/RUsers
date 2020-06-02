# Which replacement is the fastest?
# NB: results differ due to different sequence


ReplaceIn = function(UglyNames)
{
    UglyNames[UglyNames %in% "G"] = "F"
    UglyNames[UglyNames %in% "H"] = "B"
    UglyNames[UglyNames %in% "F"] = "E"
    UglyNames[UglyNames %in% "G"] = "F"
    UglyNames[UglyNames %in% "H"] = "B"
    UglyNames[UglyNames %in% "F"] = "E"
    UglyNames
}

ReplaceIfElse = function(UglyNames)
{
    UglyNames = ifelse(UglyNames == "G", "F", UglyNames)
    UglyNames = ifelse(UglyNames == "H", "B", UglyNames)
    UglyNames = ifelse(UglyNames == "F", "E", UglyNames)
    UglyNames = ifelse(UglyNames == "G", "F", UglyNames)
    UglyNames = ifelse(UglyNames == "H", "B", UglyNames)
    UglyNames = ifelse(UglyNames == "F", "E", UglyNames)
    UglyNames
}

ReplaceCase = function(UglyNames)
{
    sapply(UglyNames, function(x)switch(x, G = "F", H="B", F="E", A="A", B="B", C="C", x))
}

ReplaceIf = function(UglyNames)
{
    for (i in 1:length(UglyNames))
    {
        if (UglyNames[i] == "G") UglyNames[i]="F"
        else if (UglyNames[i] == "H") UglyNames[i]="B"
        else if (UglyNames[i] == "F") UglyNames[i]="E"
        if (UglyNames[i] == "G") UglyNames[i]="F"
        else if (UglyNames[i] == "H") UglyNames[i]="B"
        else if (UglyNames[i] == "F") UglyNames[i]="E"
    }
    UglyNames
}

ReplaceSub = function(UglyNames, ...)
{
    UglyNames = sub("G", "F", UglyNames, ...)
    UglyNames = sub("H", "B", UglyNames, ...)
    UglyNames = sub("F", "E", UglyNames, ...)
    UglyNames = sub("G", "F", UglyNames, ...)
    UglyNames = sub("H", "B", UglyNames, ...)
    UglyNames = sub("F", "E", UglyNames, ...)
    UglyNames
}

mb3 = microbenchmark(ReplaceIn(LETTER), ReplaceIfElse(LETTER), ReplaceCase(LETTER), ReplaceIf(LETTER),
               ReplaceSub(LETTER), ReplaceSub(LETTER, useBytes=TRUE), times = 10000)
plot(mb3, ylim=c(-1, 100000))
