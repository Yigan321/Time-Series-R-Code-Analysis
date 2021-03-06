USE [Forecast]
GO
/****** Object:  StoredProcedure [dbo].[sp_Create_ControlTable]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Create_ControlTable]

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

TRUNCATE TABLE rvfc.control_forecastscenario
INSERT INTO rvfc.control_forecastscenario (Scenario, Active)
      VALUES ('Order_Intake', 'X'),
      ('Revenue', '')


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Delete_Query1]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Delete_Query1] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

DELETE FROM rvfc.result_finalprediction WHERE Scenario = 'Revenue'

END

GO
/****** Object:  StoredProcedure [dbo].[sp_Delete_Query2]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Delete_Query2] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
DELETE FROM rvfc.result_prediction WHERE Scenario = 'Revenue'


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Delete_Query3]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Delete_Query3] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
DELETE FROM rvfc.result_finalranktimeseries WHERE Scenario = 'Revenue'


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Delete_Query4]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Delete_Query4] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here
DELETE FROM rvfc.output_dashboardtool WHERE Scenario = 'Revenue'

END

GO
/****** Object:  StoredProcedure [dbo].[sp_Insert_Query_1]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Insert_Query_1] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

Declare @Scenario VARCHAR(MAX)

SELECT @Scenario = Scenario FROM rvfc.control_forecastscenario WHERE(Active = 'X')

IF @Scenario = 'Order_Intake'

BEGIN

INSERT INTO rvfc.work_currencyconversionpredict (Master_Run_ID, Run_ID, Dimension1, Dimension2, Period, Point_Forecast, Lower_Level_1,Upper_Level_1, Lower_Level_2, Upper_Level_2, Rank)
SELECT rvfc.result_finalprediction.[Master_Run_ID], rvfc.result_finalprediction.Run_ID, rvfc.result_finalprediction.Dimension1, rvfc.result_finalprediction.Dimension2, rvfc.result_finalprediction.Period, rvfc.result_finalprediction.Point_Forecast, rvfc.result_finalprediction.Lower_Level_1, rvfc.result_finalprediction.Upper_Level_1, rvfc.result_finalprediction.Lower_Level_2, rvfc.result_finalprediction.Upper_Level_2, rvfc.result_finalprediction.Rank
FROM rvfc.result_finalprediction
WHERE Scenario = 'Order_Intake'

END

ELSE

BEGIN

INSERT INTO rvfc.work_currencyconversionpredict (Master_Run_ID, Run_ID, Dimension1, Dimension2, Period, Point_Forecast, Lower_Level_1,Upper_Level_1, Lower_Level_2, Upper_Level_2, Rank)
SELECT rvfc.result_finalprediction.[Master_Run_ID], rvfc.result_finalprediction.Run_ID, rvfc.result_finalprediction.Dimension1, rvfc.result_finalprediction.Dimension2, rvfc.result_finalprediction.Period, rvfc.result_finalprediction.Point_Forecast, rvfc.result_finalprediction.Lower_Level_1, rvfc.result_finalprediction.Upper_Level_1, rvfc.result_finalprediction.Lower_Level_2, rvfc.result_finalprediction.Upper_Level_2, rvfc.result_finalprediction.Rank
FROM rvfc.result_finalprediction 
WHERE Scenario = 'Revenue'

END


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Insert_Query_2]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Insert_Query_2]

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

INSERT INTO rvfc.work_currencyconversionpredict (Master_Run_ID, Run_ID, Dimension1, Dimension2, Period, Point_Forecast, Lower_Level_1,Upper_Level_1, Lower_Level_2, Upper_Level_2, Rank)
SELECT rvfc.result_finalprediction.[Master_Run_ID], rvfc.result_finalprediction.Run_ID, rvfc.result_finalprediction.Dimension1, rvfc.result_finalprediction.Dimension2, rvfc.result_finalprediction.Period, rvfc.result_finalprediction.Point_Forecast, rvfc.result_finalprediction.Lower_Level_1, rvfc.result_finalprediction.Upper_Level_1, rvfc.result_finalprediction.Lower_Level_2, rvfc.result_finalprediction.Upper_Level_2, rvfc.result_finalprediction.Rank
FROM rvfc.result_finalprediction 
WHERE Scenario = 'Revenue'

END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_ControlTable]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_ControlTable] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

Declare @Scenario VARCHAR(MAX)

SELECT @Scenario = Scenario FROM rvfc.control_forecastscenario WHERE(Active = 'X')

IF @Scenario = 'Order_Intake'

BEGIN

TRUNCATE TABLE rvfc.control_forecastscenario
INSERT INTO rvfc.control_forecastscenario (Scenario, Active)
      VALUES ('Order_Intake', ''),
      ('Revenue', 'X')

END

ELSE

BEGIN

TRUNCATE TABLE rvfc.control_forecastscenario
INSERT INTO rvfc.control_forecastscenario (Scenario, Active)
      VALUES ('Order_Intake', 'X'),
      ('Revenue', '')

END


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_Query_6]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_Query_6]

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

UPDATE rvfc.work_dashboardtool SET rvfc.work_dashboardtool.Scenario = 'Revenue' WHERE (((rvfc.work_dashboardtool.Scenario) IS NULL))

END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_Query1]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_Query1]
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

UPDATE rvfc.work_currencyconversion
SET FX_Rate = [Exchange_Rate]
FROM rvfc.staging_exchangerates
INNER JOIN (rvfc.work_currencyconversion INNER JOIN rvfc.control_dateconversioncur ON (rvfc.control_dateconversioncur.Fiscal_Year = rvfc.work_currencyconversion.Fiscal_Year) AND (rvfc.work_currencyconversion.Posting_period = rvfc.control_dateconversioncur.Posting_period)) 
ON (rvfc.staging_exchangerates.[From_Currency] = rvfc.work_currencyconversion.Currency) AND (rvfc.staging_exchangerates.[Calendar_Day] = rvfc.control_dateconversioncur.[Calendar_Day])


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_Query2]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_Query2]
	-- Add the parameters for the stored procedure here
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

UPDATE rvfc.work_currencyconversionpredict 
SET rvfc.work_currencyconversionpredict.[Currency] = [Local_Currency]
FROM rvfc.work_currencyconversionpredict 
INNER JOIN rvfc.staging_consunit ON rvfc.work_currencyconversionpredict.Dimension1 = rvfc.staging_consunit.[Consolidation_Unit] 
WHERE (((rvfc.work_currencyconversionpredict.Currency) Is Null))


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_Query3]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_Query3]
	-- Add the parameters for the stored procedure here
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

UPDATE rvfc.work_currencyconversionpredict
SET rvfc.work_currencyconversionpredict.FX_Rate = [Exchange_Rate]
FROM rvfc.staging_exchangerates 
INNER JOIN (rvfc.work_currencyconversionpredict INNER JOIN rvfc.control_dateconversioncur ON (rvfc.control_dateconversioncur.Fiscal_Year = rvfc.work_currencyconversionpredict.Fiscal_Year) AND (rvfc.work_currencyconversionpredict.Posting_period = rvfc.control_dateconversioncur.Posting_period)) 
ON (rvfc.staging_exchangerates.[From_Currency] = rvfc.work_currencyconversionpredict.Currency) AND (rvfc.staging_exchangerates.[Calendar_Day] = rvfc.control_dateconversioncur.[Calendar_Day])


END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_Query4]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_Query4]
	-- Add the parameters for the stored procedure here
AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

UPDATE rvfc.work_currencyconversionpredict
SET rvfc.work_currencyconversionpredict.FX_Rate = [Exchange_Rate]
FROM rvfc.staging_exchangerates 
INNER JOIN rvfc.work_currencyconversionpredict 
ON rvfc.staging_exchangerates.[From_Currency] = rvfc.work_currencyconversionpredict.Currency 
WHERE (((rvfc.work_currencyconversionpredict.FX_Rate) Is Null) AND ((rvfc.staging_exchangerates.[Calendar_Day])='12/31/2090 0:00:00'))

END

GO
/****** Object:  StoredProcedure [dbo].[sp_Update_Query5]    Script Date: 3/10/2021 10:05:14 PM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
-- =============================================
-- Author:		<Author,,Name>
-- Create date: <Create Date,,>
-- Description:	<Description,,>
-- =============================================
CREATE PROCEDURE [dbo].[sp_Update_Query5] 

AS
BEGIN
	-- SET NOCOUNT ON added to prevent extra result sets from
	-- interfering with SELECT statements.
	SET NOCOUNT ON;

    -- Insert statements for procedure here

Declare @Scenario VARCHAR(MAX)

SELECT @Scenario = Scenario FROM rvfc.control_forecastscenario WHERE(Active = 'X')

IF @Scenario = 'Order_Intake'

BEGIN

UPDATE rvfc.work_dashboardtool SET rvfc.work_dashboardtool.Scenario = 'Order_Intake' WHERE (((rvfc.work_dashboardtool.Scenario) IS NULL))

END

ELSE

BEGIN

UPDATE rvfc.work_dashboardtool SET rvfc.work_dashboardtool.Scenario = 'Revenue' WHERE (((rvfc.work_dashboardtool.Scenario) IS NULL))

END


END

GO
