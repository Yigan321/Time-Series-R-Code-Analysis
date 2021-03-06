USE [Forecast]
GO

/****** Object:  Schema [rvfc]    Script Date: 8/4/2020 1:53:34 PM ******/
CREATE SCHEMA [rvfc] AUTHORIZATION [dbo]
GO

/****** Object:  Table [rvfc].[control_forecastscenario]    Script Date: 3/11/2021 3:09:21 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO


IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'control_forecastscenario'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[control_forecastscenario](
	[Scenario] [varchar](255) NULL,
	[Active] [varchar](255) NULL
) ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[control_parameters]    Script Date: 3/11/2021 3:09:21 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'control_parameters'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[control_parameters](
	[Technique] [varchar](max) NULL,
	[Method] [varchar](max) NULL,
	[Threshold] [varchar](max) NULL,
	[Validation] [int] NULL,
	[Forecast_Period] [int] NULL,
	[Lower_Prediction_Interval] [int] NULL,
	[Upper_Prediction_Interval] [int] NULL,
	[Periodicity] [int] NULL,
	[Activation] [varchar](max) NOT NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[control_timescenario]    Script Date: 3/11/2021 3:09:21 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'control_timescenario'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[control_timescenario](
	[Start_Date] [varchar](255) NULL,
	[End_Date] [varchar](255) NULL,
	[Day] [varchar](255) NULL,
	[Week] [varchar](255) NULL,
	[Month] [varchar](255) NULL,
	[Year] [varchar](255) NULL
) ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[control_timethreshold]    Script Date: 3/11/2021 3:09:21 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'control_timethreshold'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[control_timethreshold](
	[Threshold] [varchar](100) NULL,
	[Lower] [int] NULL,
	[Upper] [int] NULL
) ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[work_datainput_timescenario]    Script Date: 3/11/2021 3:09:21 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_datainput_timescenario'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_datainput_timescenario](
	[Dimension1] [nvarchar](max) NULL,
	[Dimension2] [nvarchar](max) NULL,
	[Scenario] [nvarchar](max) NULL,
	[Currency] [nvarchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_year] [int] NULL,
	[Value] [float] NOT NULL,
	[Value_Euro] [float] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
/****** Object:  Table [rvfc].[work_machine_learning]    Script Date: 3/11/2021 3:09:21 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_machine_learning'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_machine_learning](
	[Dimension1] [nvarchar](max) NULL,
	[Dimension2] [nvarchar](max) NULL,
	[Dataset] [nvarchar](max) NULL,
	[Posting_period] [float] NULL,
	[Sale] [float] NULL,
	[Orderintake_lag1] [float] NULL,
	[Orderintake_lag2] [float] NULL,
	[Orderintake_lag3] [float] NULL,
	[Orderintake_lag4] [float] NULL,
	[Orderintake_lag5] [float] NULL,
	[Orderintake_lag6] [float] NULL,
	[Orderintake_lag7] [float] NULL,
	[Orderintake_lag8] [float] NULL,
	[Orderintake_lag9] [float] NULL,
	[Orderintake_lag10] [float] NULL,
	[Orderintake_lag11] [float] NULL,
	[Orderintake_lag12] [float] NULL,
	[Orderintake_MA3] [float] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO

/****** Object:  Table [rvfc].[result_timeseriesdecomposition]    Script Date: 3/11/2021 3:09:22 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_timeseriesdecomposition'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_timeseriesdecomposition](
	[Master_Run_ID] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Period] [varchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_year] [int] NULL,
	[Currency] [varchar](max) NULL,
	[Actual] [real] NULL,
	[Trend] [real] NULL,
	[Seasonality] [real] NULL,
	[Randomness] [real] NULL,
	[Rank] [int] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO

/****** Object:  Table [rvfc].[result_timeseriesdecomposition_history]    Script Date: 3/11/2021 3:09:22 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_timeseriesdecomposition_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_timeseriesdecomposition_history](
	[Master_Run_ID] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Period] [varchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_year] [int] NULL,
	[Currency] [varchar](max) NULL,
	[Actual] [real] NULL,
	[Trend] [real] NULL,
	[Seasonality] [real] NULL,
	[Randomness] [real] NULL,
	[Rank] [int] NULL,
	[Time_stamp] [varchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO

/****** Object:  Table [rvfc].[output_dashboardtool]    Script Date: 3/11/2021 3:09:22 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'output_dashboardtool'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[output_dashboardtool](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Type] [varchar](max) NULL,
	[Version] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Currency] [varchar](max) NULL,
	[FX_Rate] [real] NULL,
	[Period] [int] NULL,
	[Year] [int] NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL,
	[Rank] [int] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[output_dashboardtool_history]    Script Date: 3/11/2021 3:09:22 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'output_dashboardtool_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[output_dashboardtool_history](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Type] [varchar](max) NULL,
	[Version] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NOT NULL,
	[Scenario] [varchar](max) NULL,
	[Currency] [varchar](max) NULL,
	[FX_Rate] [real] NULL,
	[Period] [int] NULL,
	[Year] [int] NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL,
	[Rank] [int] NULL,
	[Time_stamp] [varchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO


/****** Object:  Table [rvfc].[result_finalprediction]    Script Date: 3/11/2021 3:09:22 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_finalprediction'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_finalprediction](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Period] [varchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_year] [int] NULL,
	[Point_forecast] [real] NOT NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL,
	[Rank] [int] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_finalprediction_history]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_finalprediction_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_finalprediction_history](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Period] [varchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_Year] [int] NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL,
	[Rank] [int] NULL,
	[Time_stamp] [varchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_finalranktimeseries]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_finalranktimeseries'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_finalranktimeseries](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Technique] [varchar](max) NULL,
	[Method] [varchar](max) NULL,
	[Dataset] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[MAE] [real] NULL,
	[RMSE] [real] NULL,
	[MAPE] [real] NULL,
	[Final_Rank] [real] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_finalranktimeseries_history]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_finalranktimeseries_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_finalranktimeseries_history](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Technique] [varchar](max) NULL,
	[Method] [varchar](max) NULL,
	[Dataset] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[MAE] [real] NULL,
	[RMSE] [real] NULL,
	[MAPE] [real] NULL,
	[Final_Rank] [real] NULL,
	[Time_stamp] [varchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_modelruntimeseries]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_modelruntimeseries'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_modelruntimeseries](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Technique] [varchar](max) NULL,
	[Method] [varchar](max) NULL,
	[Model_Run_Date] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Threshold] [varchar](max) NULL,
	[Train_Start_Month] [varchar](max) NULL,
	[Train_End_Month] [varchar](max) NULL,
	[Test_Start_Month] [varchar](max) NULL,
	[Test_End_Month] [varchar](max) NULL,
	[Validation] [int] NULL,
	[Forecast_Period] [int] NULL,
	[Lower_Prediction_Interval] [int] NULL,
	[Upper_Prediction_Interval] [int] NULL,
	[Local_Currency] [varchar](max) NULL,
	[Periodicity] [varchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_modelruntimeseries_history]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_modelruntimeseries_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_modelruntimeseries_history](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Technique] [varchar](max) NULL,
	[Method] [varchar](max) NULL,
	[Model_Run_Date] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Threshold] [varchar](max) NULL,
	[Train_Start_Month] [varchar](max) NULL,
	[Train_End_Month] [varchar](max) NULL,
	[Test_Start_Month] [varchar](max) NULL,
	[Test_End_Month] [varchar](max) NULL,
	[Validation] [int] NULL,
	[Forecast_Period] [int] NULL,
	[Lower_Prediction_Interval] [int] NULL,
	[Upper_Prediction_Interval] [int] NULL,
	[Local_Currency] [varchar](max) NULL,
	[Periodicity] [int] NULL,
	[Time_stamp] [varchar](max) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_prediction]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_prediction'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_prediction](
	[Master_Run_ID] [varchar](255) NULL,
	[Run_ID] [varchar](255) NULL,
	[Scenario] [varchar](255) NULL,
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[Period] [date] NULL,
	[Posting_period] [varchar](255) NULL,
	[Fiscal_year] [varchar](255) NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL
) ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_prediction_history]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_prediction_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_prediction_history](
	[Master_Run_ID] [varchar](255) NULL,
	[Run_ID] [varchar](255) NULL,
	[Scenario] [varchar](255) NULL,
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[Period] [date] NULL,
	[Posting_period] [int] NULL,
	[Fiscal_year] [varchar](255) NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL,
	[Rank] [int] NULL,
	[Time_stamp] [varchar](255) NULL
) ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_scenarios]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_scenarios'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_scenarios](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_Year] [int] NULL,
	[Currency] [varchar](max) NULL,
	[Base_Forecast] [real] NULL,
	[Business_Intuition] [real] NULL,
	[Composite_Forecast] [real] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_scenarios_history]    Script Date: 3/11/2021 3:09:23 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO
IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_scenarios_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_scenarios_history](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_Year] [int] NULL,
	[Currency] [varchar](max) NULL,
	[Base_Forecast] [real] NULL,
	[Business_Intuition] [real] NULL,
	[Composite_Forecast] [real] NULL,
	[Time_stamp] [varchar](255) NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[result_xgboostfeatureimportance]    Script Date: 3/11/2021 3:09:24 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_xgboostfeatureimportance'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_xgboostfeatureimportance](
	[Master_Run_ID] [nvarchar](255) NULL,
	[Run_ID] [nvarchar](255) NULL,
	[Dimension1] [nvarchar](255) NULL,
	[Dimension2] [nvarchar](255) NULL,
	[Feature] [nvarchar](255) NULL,
	[Gain] [real] NULL
) ON [PRIMARY]

END

GO
/****** Object:  Table [rvfc].[result_xgboostfeatureimportance_history]    Script Date: 3/11/2021 3:09:24 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'result_xgboostfeatureimportance_history'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[result_xgboostfeatureimportance_history](
	[Master_Run_ID] [nvarchar](255) NULL,
	[Run_ID] [nvarchar](255) NULL,
	[Dimension1] [nvarchar](255) NULL,
	[Dimension2] [nvarchar](255) NULL,
	[Feature] [nvarchar](255) NULL,
	[Gain] [real] NULL,
	[Time_stamp] [nvarchar](255) NULL
) ON [PRIMARY]

END

GO


/****** Object:  Table [rvfc].[datainput_forecast]    Script Date: 3/11/2021 3:09:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'datainput_forecast'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[datainput_forecast](
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[Scenario] [nvarchar](50) NULL,
	[Currency] [nvarchar](50) NULL,
	[Posting_period] [int] NULL,
	[Fiscal_year] [int] NULL,
	[Value] [float] NULL,
	[Value_Euro] [float] NULL
) ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO

/****** Object:  Table [rvfc].[work_dashboardtool]    Script Date: 3/11/2021 3:09:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_dashboardtool'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_dashboardtool](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Type] [varchar](max) NULL,
	[Version] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[Scenario] [varchar](max) NULL,
	[Currency] [varchar](max) NULL,
	[FX_Rate] [real] NULL,
	[Period] [real] NULL,
	[Year] [int] NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL,
	[Rank] [int] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO

/****** Object:  Table [rvfc].[work_finalranktimeseries]    Script Date: 3/11/2021 3:09:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_finalranktimeseries'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_finalranktimeseries](
	[Master_Run_ID] [varchar](max) NULL,
	[Run_ID] [varchar](max) NULL,
	[Technique] [varchar](max) NULL,
	[Method] [varchar](max) NULL,
	[Dataset] [varchar](max) NULL,
	[Dimension1] [varchar](max) NULL,
	[Dimension2] [varchar](max) NULL,
	[MAE] [real] NULL,
	[RMSE] [real] NULL,
	[MAPE] [real] NULL,
	[Final_Rank] [real] NULL
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[work_predictionensemble]    Script Date: 3/11/2021 3:09:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_predictionensemble'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_predictionensemble](
	[Master_Run_ID] [nvarchar](255) NULL,
	[Run_ID] [nvarchar](255) NULL,
	[Dimension1] [nvarchar](255) NULL,
	[Dimension2] [nvarchar](255) NULL,
	[Period] [nvarchar](255) NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL
) ON [PRIMARY]

END

GO
/****** Object:  Table [rvfc].[work_predictiontimeseries]    Script Date: 3/11/2021 3:09:25 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_predictiontimeseries'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_predictiontimeseries](
	[Master_Run_ID] [varchar](255) NULL,
	[Run_ID] [varchar](255) NULL,
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[Period] [varchar](255) NULL,
	[Point_Forecast] [real] NULL,
	[Lower_Level_1] [real] NULL,
	[Upper_Level_1] [real] NULL,
	[Lower_Level_2] [real] NULL,
	[Upper_Level_2] [real] NULL
) ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[work_statisticalperformance]    Script Date: 3/11/2021 3:09:26 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_statisticalperformance'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_statisticalperformance](
	[Master_Run_ID] [varchar](255) NULL,
	[Run_ID] [varchar](255) NULL,
	[Technique] [varchar](255) NULL,
	[Method] [varchar](255) NULL,
	[Dataset] [varchar](255) NULL,
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[MAE] [real] NULL,
	[RMSE] [real] NULL,
	[MAPE] [real] NULL
) ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[work_statisticsensemble]    Script Date: 3/11/2021 3:09:26 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_statisticsensemble'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_statisticsensemble](
	[Master_Run_ID] [varchar](255) NULL,
	[Run_ID] [varchar](255) NULL,
	[Technique] [varchar](255) NULL,
	[Method] [varchar](255) NULL,
	[Dataset] [varchar](255) NULL,
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[MAE] [real] NULL,
	[RMSE] [real] NULL,
	[MAPE] [real] NULL
) ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
/****** Object:  Table [rvfc].[work_validationtimeseries]    Script Date: 3/11/2021 3:09:26 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_PADDING ON
GO

IF NOT EXISTS (SELECT *
    FROM   INFORMATION_SCHEMA.TABLES
    WHERE  TABLE_NAME = 'work_validationtimeseries'
    AND TABLE_SCHEMA = 'rvfc')
BEGIN
CREATE TABLE [rvfc].[work_validationtimeseries](
	[Run_ID] [varchar](255) NULL,
	[Method] [varchar](255) NULL,
	[Dataset] [varchar](255) NULL,
	[Dimension1] [varchar](255) NULL,
	[Dimension2] [varchar](255) NULL,
	[Point_forecast] [real] NULL,
	[Actual_value] [real] NULL
) ON [PRIMARY]

END

GO
SET ANSI_PADDING OFF
GO
