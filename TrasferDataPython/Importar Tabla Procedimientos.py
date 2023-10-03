
'''
select  column_name|| ' ' || data_type|| ' (' || c.data_length || '),'  from dba_tab_cols c 
where owner='NORTHWIND' and table_name ='CUSTOMERS'

'''
import conexion as cnn

oracle_connection = cnn.ora_conndb()
oracle_cursor = oracle_connection.cursor()
sql_server_connection = cnn.mssql_conndb()
sql_server_cursor = sql_server_connection.cursor()

def table_property(object_name,opcion):
    result=None
    if opcion==1:
        oracle_cursor.execute(f"SELECT COUNT(*) FROM user_tab_columns WHERE table_name = '{object_name}'")
        result = oracle_cursor.fetchone()[0]
    if opcion==2:
        oracle_cursor.execute(f"SELECT column_name FROM user_tab_columns WHERE table_name = '{object_name}'")
        result = [row[0] for row in oracle_cursor.fetchall()]
    if opcion==3:
        oracle_cursor.execute(f"SELECT table_name FROM user_tables")
        result = [row[0] for row in oracle_cursor.fetchall()]

    return result
    
def insert_data(table_name):
    column_count=table_property(table_name,1)
    column_names=table_property(table_name,2)
    insert_query = f"INSERT INTO northwind.{table_name} ({', '.join(column_names)}) VALUES ({', '.join(['?'] * column_count)})"
    return insert_query

def get_data(table_name):
    oracle_cursor.execute(f"select * from {table_name}")
    data = oracle_cursor.fetchall()
    return data

tables =table_property('NORTHWIND',3)
for table in tables:
    insert_query=insert_data(table)
    rows=get_data(table)
    for row in rows:
        try:
            sql_server_cursor.execute(insert_query, row)
        except Exception as e:
            print(f"Error al insertar fila: {e}")

    
sql_server_connection.commit()
print("Datos insertados correctamente en SQL Server")

oracle_cursor.close()
oracle_connection.close()
sql_server_cursor.close()
sql_server_connection.close()
