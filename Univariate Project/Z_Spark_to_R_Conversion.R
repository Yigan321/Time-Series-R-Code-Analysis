#############################################################################
# Advanced Forecasting System                       Author: Derek Kane 8/2020
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spark to R Conversion Code
#############################################################################


# Connect to SQL Server with ODBC method

library(RODBC)

SQLServer_Forecast <- odbcConnect(dsn="Forecast", uid="soothsayersql@ssasqlserver", pwd="SoothDez@413")

install.packages("DBI")
install.packages("odbc")

library(odbc)

con <- dbConnect(odbc(), "ssarfdb.eastus.cloudapp.azure.com")

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "ssarfdb.eastus.cloudapp.azure.com", 
                 Database = "Forecast", 
                 Trusted_Connection = "True")

rvfc.control_algorithms <- sqlFetch(SQLServer_Forecast, "rvfc.control_algorithms")

# We always have to attach the data frame in R in order to make use of it later.
#-------------------------------------------------------

attach(rvfc.control_algorithms)

# This is a check to see the first 10 lines of the data.

head(rvfc.control_algorithms, n=10)

# This is how we write an insert query in RODBC

SQL_Query_Insert <- "INSERT INTO rvfc.z_control_algorithms (Threshold, Method, Technique, Library) SELECT Threshold, Method, Technique, Library FROM rvfc.control_algorithms" 
resultset <- sqlQuery(SQLServer_Forecast, SQL_Query_Insert)

# This is how we write an update query in RODBC

SQL_Query_Update <- "UPDATE rvfc.z_control_algorithms SET Threshold = 'booyah'"
resultset <- sqlQuery(SQLServer_Forecast, SQL_Query_Update)

# This is how we write a truncate query in RODBC

SQL_Query_Truncate <- "TRUNCATE TABLE rvfc.z_control_algorithms"
resultset <- sqlQuery(SQLServer_Forecast, SQL_Query_Truncate)


# Close the connection to the SQL server

odbcClose(SQLServer_Forecast)


#ALternate way to write a query


sqlQuery(SQLServer_Forecast, "TRUNCATE TABLE rvfc.z_control_algorithms")


# Append into a Table from a DF

sqlSave(SQLServer_Forecast, rvfc.control_algorithms, tablename = "rvfc.z_control_algorithms", append = TRUE, rownames = FALSE)
sqlSave(SQLServer_Forecast, rvfc.control_algorithms, tablename = "rvfc.z_control_algorithms", append = TRUE, rownames = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is how we write an update query in RODBC

sql="update mytable set column=value output inserted.column where column=value"
cn <-odbcDriverConnect(connection="yourconnectionstring")
resultset <- sqlQuery(cn,sql)





# This is sample code to append into th SQL Table
# sqlUpdate(channel=SQLServerFDM, dat=BUAssignment, tablename="Business_Unit_R", index="PSC_Code")


#Database connection via RJDBC

library(RJDBC)

drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver" , "C:/Program Files/sqljdbc_8.4/enu/mssql-jdbc-8.4.0.jre8" ,identifier.quote="`")
conn <- dbConnect(drv, "jdbc:THEKANES\SQLSERVER;databaseName=forecast", "sa", "password")
d <- dbGetQuery(conn, "select * from sys.databases where database_id <= 4 ")
summary(d)






drv <- JDBC("com.mysql.jdbc.Driver",
            "/etc/jdbc/mysql-connector-java-3.1.14-bin.jar",
            identifier.quote="`")






jdbcUsername <- "sql_user"
jdbcPassword <- "sqluser_password"
jdbcHostname <- "sqlserver_host"
jdbcDatabase <- "sql_database"
jdbcServerName <- "sqlserver_name"
jdbcPort <- "1433"
jdbcUrl <- paste0("jdbc:sqlserver://",jdbcHostname,":",jdbcPort,";database=",jdbcDatabase,";user=",jdbcUsername,"@",jdbcServerName,";password=", jdbcPassword,";encrypt=true;trustServerCertificate=false;hostNameInCertificate=*.database.windows.net;loginTimeout=30;")
print(jdbcUrl)


drv <- JDBC("com.mysql.jdbc.Driver",
            "/etc/jdbc/mysql-connector-java-3.1.14-bin.jar",
            identifier.quote="`")
conn <- dbConnect(drv, "jdbc:mysql://localhost/test", "user", "pwd")