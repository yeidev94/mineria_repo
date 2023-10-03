import pyodbc
from sqlalchemy import create_engine,text
import cx_Oracle

def mssql_conndb():
    # Parámetros de conexión a la base de datos
    server ='.' #input('Ingrese el servidor de la base de datos SQL server  :')
    database ='staging' #input('Ingrese el nombre de la base de datos  SQL server  :  ')
    username ='sa' # input('Ingrese el usuario de la base de datos  SQL server  : ')
    password ='Admin12345' #input('Ingrese el contraseña de la base de datos  SQL server   : ')
    connection_string = f'DRIVER={{SQL Server}};SERVER={server};DATABASE={database};UID={username};PWD={password}'
    conn = pyodbc.connect(connection_string)
    return (conn)


def ora_conndb():
    server ='127.0.0.1'  #input('Ingrese el IP del servidor de la base de datos Oracle :')
    database ='xe'  #input('Ingrese el nombre de la base de datos  Oracle :  ')
    username ='northwind' #input('Ingrese el usuario de la base de datos  Oracle : ')
    password ='Admin12345' #input('Ingrese el contraseña de la base de datos  Oracle : ')
    dsn = cx_Oracle.makedsn(server, '1521', service_name=database)
    connection = cx_Oracle.connect(user=username, password=password, dsn=dsn)
    return connection