# install.packages("RJDBC",dep=TRUE)
library(RJDBC)

# jdbc sql connector 
drv <- JDBC("com.mysql.jdbc.Driver","/usr/local/jdbc/mysql-connector-java-5.0.8-bin.jar",
            identifier.quote="`")

# connection
conn <- dbConnect(drv, "jdbc:mysql://localhost/dbname", "username", "password")

dbListTables(conn)

# ~~~~~~~~~~~~~~~~~~~~~~~~~
options(scipen=500)
options( java.parameters = "-Xmx4g" )

numberofrows <- 25000
# number of rows using dbGetQuery
count <- dbGetQuery(conn, "select count(*) from db")
count

noOfIterations <- (count/numberofrows)+1
noOfIterations <- as.integer(noOfIterations)
noOfIterations

sqldata <- data.frame()
i <- 0
filenamelist <- c()

# loop for making dat files from SQL server
for(i in 1:noOfIterations){
  
  query <- paste("SELECT * FROM schema LIMIT", as.character(numberofrows),"OFFSET", as.character(i*numberofrows), sep=" ")
  conn <- dbConnect(drv, "jdbc:mysql://localhost/db", "username", "password")
  if(dim(sqldata)[1] == 0)
    sqldata <-  dbGetQuery(conn, query)
  else
    sqldata <- rbind(sqldata, dbGetQuery(conn, query))
  
  if((i == (noOfIterations-1)) || (dim(sqldata)[1] == numberofrows*4) )
  {
    filename <- paste("data_", as.character(i), ".dat", sep="")
    save(sqldata,  file = filename)
    filenamelist <- c(filenamelist, filename)
    rm(sqldata)
    gc()
    sqldata <- data.frame()
  }
  i <- i+1
  dbDisconnect(conn)
}
