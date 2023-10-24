
CREATE TABLE [dbo].[dim_tiempo](
	[id_tiempo] [int] IDENTITY(1,1) CONSTRAINT [pk_tiempo] PRIMARY KEY CLUSTERED ,
	[fecha] [date] NULL,
	[anio] [varchar](4) NULL,
	[mes] [varchar](2) NULL,
	[dia] [varchar](2) NULL,
	[dia_semana] [varchar](1) NULL,
	[nombre_mes] [varchar](20) NULL,
	[trimestre] [varchar](1) NULL,
	[semestre] [varchar](1) NULL,
	[mes_anio] [varchar](7) NULL,
	[periodo] [varchar](20) NULL)


CREATE TABLE [dbo].[dim_cliente](
	[id_cliente] [int] IDENTITY(1,1) CONSTRAINT [pk_cliente] PRIMARY KEY CLUSTERED,
	[bk_identificacion] [varchar](30) NULL,
	[nombre_cliente] [varchar](100) NULL,
	[pais] [varchar](50) NULL,
	[ciudad] [varchar](50) NULL,
	[estado] [varchar](50) NULL,
	[categoria_cliente] [varchar](50) NULL,
	[fecha_ingreso] [date] NULL )


CREATE TABLE [dbo].[dim_empleado](
	[id_empleado] [int] IDENTITY(1,1) CONSTRAINT [pk_empleado] PRIMARY KEY CLUSTERED ,
	[bk_identificacion] [varchar](30) NULL,
	[nombre_empleado] [varchar](100) NULL,
	[ciudad] [varchar](50) NULL,
	[pais] [varchar](50) NULL,
	[puesto] [varchar](50) NULL,
	[supervisor] [varchar](80) NULL) )


CREATE TABLE [dbo].[dim_producto](
	[id_producto] [int] IDENTITY(1,1) CONSTRAINT [pk_producto] PRIMARY KEY CLUSTERED ,
	[bk_identificacion] [varchar](20) NULL,
	[nombre_producto] [varchar](255) NULL,
	[proveedor] [varchar](100) NULL,
	[categoria] [varchar](50) NULL,
	[precio_unidad] [decimal](18, 2) NULL,
	[stock_actual] [int] NULL,
	[estado] [varchar](20) NULL)
 
  
CREATE TABLE [dbo].[fact_ventas](
	[id_venta] [int] IDENTITY(1,1) CONSTRAINT [pk_hventas] PRIMARY KEY CLUSTERED ,
	[id_cliente] [int] NULL,
	[id_producto] [int] NULL,
	[id_empleado] [int] NULL,
	[fecha] [date] NULL,
	[cantidad] [int] NULL,
	[monto] [decimal](18, 2) NULL,
	[descuento] [decimal](18, 2) NULL)
 
ALTER TABLE [dbo].[fact_ventas]  WITH CHECK ADD FOREIGN KEY([id_cliente])
REFERENCES [dbo].[dim_cliente] ([id_cliente])

ALTER TABLE [dbo].[fact_ventas]  WITH CHECK ADD FOREIGN KEY([id_empleado])
REFERENCES [dbo].[dim_empleado] ([id_empleado])

ALTER TABLE [dbo].[fact_ventas]  WITH CHECK ADD FOREIGN KEY([id_producto])
REFERENCES [dbo].[dim_producto] ([id_producto])

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
