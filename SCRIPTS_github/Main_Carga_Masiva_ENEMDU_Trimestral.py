# -*- coding: utf-8 -*-
"""
Created on Wed Jul 10 09:16:57 2024

@author: MARCELO
"""

import os
import zipfile
import pandas as pd
import pyreadstat
import psycopg2
from sqlalchemy import create_engine

def extract_zip(zip_path, extract_to='.'):
    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extractall(extract_to)

def read_spss(file_path):
    df, meta = pyreadstat.read_sav(file_path)
    return df

def save_to_postgresql(df, table_name, conn_params):
    engine = create_engine(f"postgresql+psycopg2://{conn_params['user']}:{conn_params['password']}@{conn_params['host']}:{conn_params['port']}/{conn_params['dbname']}")
    df.to_sql(table_name, engine, if_exists='replace', index=False)

def main(zip_path, extract_to, conn_params):
    # Extract all files from zip
    extract_zip(zip_path, extract_to)
    
    # Process each extracted file
    for root, dirs, files in os.walk(extract_to):
        for file in files:
            if file.endswith('.sav'):
                file_path = os.path.join(root, file)
                df = read_spss(file_path)
                
                # Determine table name based on file name
                if 'personas' in file.lower():
                    table_name = 'personas_table'
                elif 'viviendas' in file.lower():
                    table_name = 'viviendas_table'
                else:
                    table_name = 'other_table'
                
                # Save dataframe to PostgreSQL
                try:
                    save_to_postgresql(df, table_name, conn_params)
                    print(f"Data saved to {table_name} successfully.")
                except Exception as e:
                    print(f"Failed to save data to {table_name}: {e}")

if __name__ == '__main__':
    zip_path = r'D:/MDMQ/MDMQ_DWA/DB_onedrive/ENEMDU_TRIMESTRAL/'
    extract_to = r'D:/MDMQ/MDMQ_DWA/DB_onedrive/ENEMDU_TRIMESTRAL/'
    conn_params = {
        'dbname': 'sidep',
        'user': 'postgres',
        'password': 'marce',
        'host': 'localhost',
        'port': '5432'
    }
    main(zip_path, extract_to, conn_params)