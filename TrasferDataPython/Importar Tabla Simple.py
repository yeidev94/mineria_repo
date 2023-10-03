
'''

select  column_name|| ' ' || data_type|| ' (' || c.data_length || '),'  from dba_tab_cols c 
where owner='NORTHWIND' and table_name ='CUSTOMERS'

'''

import pandas as pd
import conexion as cnn

oracle_connection = cnn.ora_conndb()
oracle_cursor = oracle_connection.cursor()

sql_server_connection = cnn.mssql_conndb()
sql_server_cursor = sql_server_connection.cursor()

oracle_cursor.execute("SELECT COUNT(*) FROM user_tab_columns WHERE table_name = 'CUSTOMERS'")
column_count = oracle_cursor.fetchone()[0]

# Obtener los nombres de las columnas
oracle_cursor.execute("SELECT column_name FROM user_tab_columns WHERE table_name = 'CUSTOMERS'")
column_names = [row[0] for row in oracle_cursor.fetchall()]

insert_query = f"INSERT INTO customers ({', '.join(column_names)}) VALUES ({', '.join(['?'] * column_count)})"

oracle_cursor.execute("select * from customers")
rows = oracle_cursor.fetchall()

for row in rows:
    sql_server_cursor.execute(insert_query, row)
    
sql_server_connection.commit()
print("Datos insertados correctamente en SQL Server")


#print(column_count)
#print(column_names)
#print(insert_query)
oracle_cursor.close()
oracle_connection.close()
sql_server_cursor.close()
sql_server_connection.close()

