import os
import string

# Obtener el directorio actual del script
current_directory = os.path.dirname(os.path.abspath(__file__))

# Crear carpetas con el formato "Letra A", "Letra B", ..., "Letra O"
for letter in string.ascii_uppercase[:15]:  # Hasta la letra O (15 letras)
    folder_name = f"Letra {letter}"
    folder_path = os.path.join(current_directory, folder_name)
    os.makedirs(folder_path, exist_ok=True)
    
    # Crear el archivo placebo.txt en cada carpeta
    file_path = os.path.join(folder_path, "placebo.txt")
    with open(file_path, 'w') as f:
        f.write("Este es un archivo placebo.")
    
    print(f"Carpeta creada: {folder_path} con archivo placebo.txt")

print("Todas las carpetas y archivos se han creado correctamente.")
