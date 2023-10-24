
select *  from  NORTHWIND.CATEGORIES
select *  from  NORTHWIND.CUSTOMERS
select *  from  NORTHWIND.EMPLOYEES
select *  from  NORTHWIND.ORDER_DETAILS
select *  from  NORTHWIND.ORDERS
select *  from  NORTHWIND.PRODUCTS
select *  from  NORTHWIND.SHIPPERS
select *  from  NORTHWIND.SUPPLIERS

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

select *  from JARDINERIA.CLIENTE
select *  from  JARDINERIA.DETALLE_PEDIDO 
select *  from JARDINERIA.EMPLEADO 
select *  from  JARDINERIA.GAMA_PRODUCTO
select *  from  JARDINERIA.OFICINA 
select *  from JARDINERIA.PAGO
select *  from JARDINERIA.PEDIDO 
select *  from JARDINERIA.PRODUCTO 

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

select *  from utndw.dbo.dim_cliente
select *  from utndw.dbo.dim_producto
select *  from utndw.dbo.dim_empleado
select *  from utndw.dbo.dim_tiempo
select *  from utndw.dbo.fact_ventas

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
create  view v_cliente as 
select bc.bk_identificacion,
       upper(bc.nombre_cliente) cliente,
       upper(bc.pais) pais,
       upper(bc.ciudad) ciudad,
       iif(cliente is null, 'INACTIVO', 'ACTIVO') estado,
      ISNULL(
	  case
         when categoria = 1 then
          'SOBRESALIENTE'
         when categoria = 2 then
          'BUENO'
         when categoria = 3 then
          'REGULAR'
         when categoria = 4 then
          'NORMAL'
       end,'INACTIVO') categoria_cliente,
	   fecha_ingreso
  from (select concat(1, CUSTOMER_id) bk_identificacion,
               COMPANY_NAME nombre_cliente,
               COUNTRY pais,
               CITY ciudad
          from NORTHWIND.CUSTOMERS
        union all
        select concat(2, CODIGO_CLIENTE), NOMBRE_CLIENTE, PAIS, CIUDAD
          from JARDINERIA.CLIENTE) bc
  left join (select cliente, NTILE(4) OVER(ORDER BY total DESC) categoria,fecha_ingreso
               from (select concat(1, customer_id) cliente,
                            sum(UNIT_PRICE * QUANTITY) total, MIN(ORDER_DATE)  fecha_ingreso
                       from northwind.orders o
                      inner join northwind.order_details od
                         on o.order_id = od.order_id
						 group by  concat(1, customer_id) 
                     union all
                     select concat(2, codigo_cliente) customerid,
                          SUM(  PRECIO_UNIDAD * CANTIDAD) total, MIN(fecha_pedido) fecha_ingreso
                       from jardineria.pedido o
                      inner join jardineria.detalle_pedido od
                         on o.codigo_pedido = od.codigo_pedido
						 group by concat(2, codigo_cliente)
						 ) c) cc
    on bc.bk_identificacion = cc.cliente

	select *  from  utndw.dbo.dim_cliente 

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

insert into utndw.dbo.dim_cliente 
select *  from v_cliente

select *  from utndw.dbo.dim_cliente 

--xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

create view v_productos as 
select cast(PRODUCT_ID as varchar(5)) identificador,
       upper(PRODUCT_NAME) producto,
       upper(s.company_name) proveedor,
       upper(c.category_name) categoria,
       UNIT_PRICE precio_unidad,
       UNITS_IN_STOCK cantida_stock,
       case
         when DISCONTINUED = 'N' then
          'ACTIVO'
         else
          'DESCONTINUADO'
       end estado
  from NORTHWIND.PRODUCTS pr
 inner join NORTHWIND.SUPPLIERS s
    on pr.supplier_id = s.supplier_id
 inner join NORTHWIND.CATEGORIES c
    on c.category_id = pr.category_id
union all
select 
 codigo_producto identificador,
       upper(nombre) producto,
       upper(proveedor) proveedor,
       upper(gama) categoria,
       precio_venta precio_unidad,
       CANTIDAD_EN_STOCK cantida_stock,
         'ACTIVO' estado
  from jardineria.producto


select *  from v_productos

insert into utndw.dbo.dim_producto
select *  from v_productos

select *  from utndw.dbo.dim_producto
---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


create view v_empleado  as
	select concat(1,a.employee_id) codigo_empleado,
       upper(concat(a.firstname, ' ', a.lastname)) nombre,
       UPPER( a.CITY) ciudad,
       upper(a.COUNTRY) pais,
       upper(a.TITLE) puesto,
       upper(concat(B.firstname, ' ', B.lastname)) jefe
  from northwind.employees a
 left  JOIN northwind.employees B
    ON A.REPORTS_TO = B.EMPLOYEE_ID
union all
	select concat(2,e.codigo_empleado),
       upper(concat(e.nombre, ' ', e.apellido1,' ',e.apellido2)) nombre,
 
 UPPER( o.ciudad) ciudad,
       upper(o.pais) pais,
       upper(e.puesto) puesto,
       upper(concat(j.nombre, ' ', j.apellido1,' ',j.apellido2))  jefe
  from jardineria.empleado e
  inner join jardineria.oficina o
  on e.codigo_oficina=o.codigo_oficina
 left  JOIN jardineria.empleado j
    ON e.codigo_jefe = j.codigo_empleado
 

 select *  from  utndw.dbo.dim_empleado
 insert into  utndw.dbo.dim_empleado
 select *  from v_empleado

---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
select *  from  utndw.dbo.fact_ventas
select * from v_ventas
create  view v_ventas as 

select concat(1, customer_id) cliente,
       concat(1, o.employee_id) empleado,
       cast(product_id as varchar(5)) producto,
       ORDER_DATE fecha,
       sum(QUANTITY) cantidad,
	   sum(UNIT_PRICE) monto,
	   sum(DISCOUNT) descuento
       
  from northwind.orders o
 inner join northwind.order_details od
    on o.order_id = od.order_id
 group by concat(1, customer_id),
          concat(1, o.employee_id),
          ORDER_DATE,
          product_id
union all
select concat(2, cl.codigo_cliente) cliente,
       concat(2, CODIGO_EMPLEADO_REP_VENTAS) empleado,
       codigo_producto producto,
       fecha_pedido,
       sum(CANTIDAD) cantidad,
	   sum(PRECIO_UNIDAD) monto,
	   0 descuento
  from jardineria.pedido o
 inner join jardineria.detalle_pedido od
    on o.codigo_pedido = od.codigo_pedido
  left join jardineria.cliente cl
    on o.codigo_cliente = cl.codigo_cliente
 group by concat(2, cl.codigo_cliente),
          concat(2, CODIGO_EMPLEADO_REP_VENTAS),
          fecha_pedido,
          codigo_producto


---- llenar la tabla de tiempos

  select *  from utndw.dbo.dim_tiempo
  delete  from utndw.dbo.dim_tiempo
  
SET LANGUAGE Spanish;
DECLARE @fecha_inicio DATE;-- = '2020-01-01';
DECLARE @fecha_fin DATE ;--= '2023-12-31';

select @fecha_inicio= MIN(fecha),@fecha_fin= MAX(fecha)  from v_ventas

WHILE @fecha_inicio <= @fecha_fin
BEGIN
INSERT INTO utndw.dbo.dim_tiempo ( fecha, anio, mes, dia, dia_semana, nombre_mes, trimestre, semestre, mes_anio,periodo)
    VALUES ( @fecha_inicio, YEAR(@fecha_inicio), 
			MONTH(@fecha_inicio), 
			DAY(@fecha_inicio),
            DATEPART(WEEKDAY, @fecha_inicio), DATENAME(MONTH, @fecha_inicio), 
            DATEPART(QUARTER, @fecha_inicio), (MONTH(@fecha_inicio) + 2) / 3, 
            CONVERT(VARCHAR(7), @fecha_inicio, 126),
			concat(substring(CONVERT(VARCHAR(7), @fecha_inicio, 126),6,2),' - ',DATENAME(MONTH, @fecha_inicio)))

        SET @fecha_inicio = DATEADD(DAY, 1, @fecha_inicio);
END


-- Se carga la tabla de Hechos 

insert into  utndw.dbo.fact_ventas(id_cliente,id_producto,id_empleado,fecha,cantidad,monto,descuento)
select c.id_cliente,p.id_producto,e.id_empleado,v.fecha ,v.cantidad ,v.monto,v.descuento
from v_ventas v
inner join utndw.dbo.dim_cliente c
on v.cliente=c.bk_identificacion
inner join utndw.dbo.dim_empleado e
on v.empleado=e.bk_identificacion
inner join utndw.dbo.dim_producto p
on v.producto=p.bk_identificacion
inner join utndw.dbo.dim_tiempo t
on cast(v.fecha as date)=cast(t.fecha as date)

select *  from  utndw.dbo.fact_ventas

-- Por ultimo se hace el cubo y se conecta a excel 
