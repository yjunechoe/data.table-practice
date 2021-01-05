Prep
----

    read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
      n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
      if (auto.colnames){
        cols <- c()
        con <- file(filepath, "r")
        while ( TRUE ) {
          line <- readLines(con, n = 1, warn=FALSE)
          if ( length(line) == 0) {
            break
          }
          m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
          if (length(m) == 3) {
            index <- as.numeric(m[2])
            value <- m[3]
            if (index < length(cols)){
              cols <- c()
            }
            if (is.function(fun.col)){
             cols <- fun.col(value,cols)
            }
            cols[index] <- value
            if (index == n.cols){
              break
            }
          }
        }
        close(con)
        return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
      }
      else{
        return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
      }
    }
    data <- read.pcibex('https://raw.githubusercontent.com/yjunechoe/sandbox/master/data/BA_thesis_result.txt')

tidyverse
---------

    library(tidyverse)

    thesis_tbl <- as_tibble(data) %>% 
      select(Type, PennElementType, Parameter, Value, EventTime, Item, Cond, Group, Participant_ID) %>% 
      rename(Subject = Participant_ID) %>% 
      filter(PennElementType %in% c('Selector', 'Timer'), Type == 'experiment') %>% 
      select(-c("Type")) %>% 
      arrange(Item) %>% 
      filter(EventTime != "Never") %>% 
      mutate(
        Parameter = if_else(Parameter == "Start", "Start", "End"),
        EventTime = as.numeric(EventTime)
      ) %>% 
      pivot_wider(names_from = Parameter, values_from = EventTime) %>% 
      group_by(Subject, Item, Cond, Group) %>% 
      summarize(
        Value = Value[PennElementType == "Selector"],
        Time = End[2] - Start[1],
        .groups = 'drop'
      ) %>% 
      inner_join(
        read_csv("https://raw.githubusercontent.com/yjunechoe/Semantic-Persistence/master/data/Key.csv"),
        by = "Item"
      ) %>% 
      select(-c(QuestionTarget, Pitch)) %>% 
      mutate(
        Accuracy = as.numeric(Answer == Value),
        logRT = log(Time),
        Type = case_when(
          str_detect(Item, "^f") ~ "Filler",
          str_detect(Item, "^Catch") ~ "Catch",
          TRUE ~ "Critical"),
      )

    arrange(head(thesis_tbl), Item, Subject)

    ## # A tibble: 6 x 10
    ##   Subject          Item    Cond   Group Value  Time Answer Accuracy logRT Type  
    ##   <chr>            <chr>   <chr>  <chr> <chr> <dbl> <chr>     <dbl> <dbl> <chr> 
    ## 1 5699974f25d9e90~ Awaken~ Subje~ B     No     1782 No            1  7.49 Criti~
    ## 2 5699974f25d9e90~ Calmed  Subje~ B     Yes    1548 No            0  7.34 Criti~
    ## 3 5699974f25d9e90~ CatchNo Subje~ B     No      534 No            1  6.28 Catch 
    ## 4 5699974f25d9e90~ CatchY~ Verb   B     Yes     461 Yes           1  6.13 Catch 
    ## 5 5699974f25d9e90~ Choked  Subje~ B     No     2524 No            1  7.83 Criti~
    ## 6 5699974f25d9e90~ Dressed Verb   B     No     1849 No            1  7.52 Criti~

data.table
----------

    library(data.table)

    thesis_dt <- as.data.table(data)

    thesis_dt <- thesis_dt[, .(Type, PennElementType, Parameter, Value, EventTime, Item, Cond, Group, Subject = Participant_ID)]

    thesis_dt <- thesis_dt[(PennElementType %in% c('Selector', 'Timer') & Type == 'experiment' & EventTime != "Never"), -"Type"]

    thesis_dt[order(Item), Parameter := fifelse(Parameter == "Start", "Start", "End")]

    thesis_dt <- dcast(thesis_dt, PennElementType + Value + Item + Cond + Group + Subject ~ Parameter, value.var = "EventTime")

    thesis_dt <- thesis_dt[, list(Value = Value[PennElementType == "Selector"], Time = as.numeric(End)[1] - as.numeric(Start)[2]), by = c("Subject", "Item", "Cond", "Group")]

    thesis_dt <- merge(thesis_dt, fread("https://raw.githubusercontent.com/yjunechoe/Semantic-Persistence/master/data/Key.csv"), by = "Item")

    thesis_dt <- thesis_dt[, -c("QuestionTarget", "Pitch")]

    thesis_dt[, `:=`(
      Accuracy = as.numeric(Answer == Value),
      logRT = log(Time),
      Type = fcase(
        Item %like% "^f", "Filler",
        Item %like% "Catch", "Catch",
        default = "Critical")
    )]

    thesis_dt <- na.omit(thesis_dt, cols = "Value")

    arrange(head(thesis_dt), Item, Subject)

    ##        Item                  Subject    Cond Group Value Time Answer Accuracy
    ## 1: Awakened 5699974f25d9e90006b08be4 Subject     B    No 1782     No        1
    ## 2: Awakened 56ab103610b779000bcb4cb5 Subject     B    No 2252     No        1
    ## 3: Awakened 599ec7dc617ca80001fcd124 Subject     B    No 1716     No        1
    ## 4: Awakened 5a49d6a06d85f80001c25bc4 Subject     B    No 1048     No        1
    ## 5: Awakened 5a791168f49c9a0001f31061 Subject     B    No 2203     No        1
    ## 6: Awakened 5ac6cc039534ba0001c75642 Subject     B    No 3667     No        1
    ##       logRT     Type
    ## 1: 7.485492 Critical
    ## 2: 7.719574 Critical
    ## 3: 7.447751 Critical
    ## 4: 6.954639 Critical
    ## 5: 7.697575 Critical
    ## 6: 8.207129 Critical

benchmark
---------

    microbenchmark::microbenchmark(ver_tidy(), ver_dt())

    ## Unit: milliseconds
    ##        expr      min       lq      mean    median        uq      max neval cld
    ##  ver_tidy() 171.3779 241.3319 268.22356 265.06240 301.63985 397.2564   100   b
    ##    ver_dt()  46.1372  67.2469  77.48836  76.64145  88.53775 138.7310   100  a
