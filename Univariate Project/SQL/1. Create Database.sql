IF NOT EXISTS(SELECT * FROM sys.databases WHERE name = 'Forecast')
  BEGIN
    CREATE DATABASE [Forecast]
    END
    GO
       USE [Forecast]
    GO