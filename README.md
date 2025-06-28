# ACFortFormat

Utilidad de lectura y escritura estilo Fortran en Python.  
Soporte para formatos fijos, lectura libre, inferencia de tipo y salida formateada.<br>
Instalación directa vía script install.py o archivo `.whl`.

---

## Instalación recomendada

Puedes instalar automáticamente ACFortFormat ejecutando:

```bash
curl -O https://raw.githubusercontent.com/acdeveloper-sci/acfortformat/main/install.py
python install.py
```

Este script detectará tu sistema operativo y versión de Python, descargará el `.whl` apropiado desde los Releases de este repositorio, y lo instalará con `pip`.

> Requiere tener conexión a internet, `pip` disponible, y Python ≥ 3.7.

---

## Instalación manual

También puedes instalar directamente el archivo `.whl` adecuado desde la sección [Releases](https://github.com/acdeveloper-sci/acfortformat/releases):

### Windows (Python 3.8)

```bash
pip install https://github.com/acdeveloper-sci/acfortformat/releases/download/v0.1/acfortformat-0.1-cp38-cp38-win_amd64.whl
```

### Linux (Python 3.8)

```bash
pip install https://github.com/acdeveloper-sci/acfortformat/releases/download/v0.1/acfortformat-0.1-cp38-cp38-linux_x86_64.whl
```

Puedes verificar tu versión de Python con:

```bash
python --version
```

y descarga:
- acfortformat-0.1-cp[version]-cp[version]-linux_x86_64.whl para Linux
- acfortformat-0.1-cp[version]-cp[version]-win_amd64.whl para Windows

---


## Fortran-like I/O Utility – Funcionalidades clave

### LECTURA

`read(fp, fmt=None)` emula la instrucción `READ` de Fortran:

- `fmt=None`  → comportamiento como `READ(*,*)` (lectura libre):
  - Soporta enteros, flotantes (incluye notación `D`), cadenas.
  - Cadenas entre comillas dobles o simples se toman como un solo valor.
  - Inferencia automática de tipo: `int`, `float`, `str`.

- `fmt="*"`   → equivalente explícito de `READ(*,*)`.

- `fmt="(A)"` → se lee la línea completa como una sola cadena.

- `fmt="(Fortran format)"` → lectura con formato fijo (vía `fortranformat`):
  - Admite `I`, `F`, `A`, `X`, `Tn`, repeticiones, etc.
  - Compatible con alineaciones, espaciados y columnas fijas.
  - Usa caché de *readers* para mayor eficiencia.

---

### ESCRITURA

`write(data, fp=None, fmt=None, con=False)` emula la instrucción `WRITE` de Fortran:

- `fmt=None` o `fmt="*"` → comportamiento como `WRITE(*,*)`:
  - Los elementos de `data` (lista o tupla) se imprimen separados por espacio.
  - Si `data` es string o número simple, se imprime directamente.

- `fmt="(Fortran format)"` → escritura con formato fijo:
  - Admite precisión numérica (`F10.4`, `I4`, etc.), alineación, texto embebido.
  - Compatible con formatos complejos.
  - Usa caché de *writers* para optimizar.

---

### UTILIDADES ADICIONALES

- `infer_type(s)`         → convierte string a `int`, `float`, o lo deja como `str`.
- `line_sanity(line)`     → expande tabs a espacios para lecturas fijas.
- `EndOfFile`             → excepción personalizada de fin de archivo.
- Soporte transparente de archivos (`fp`) y consola (`con=True` en `write()`).

---

## Cobertura de tests incluidos

- Escritura y lectura con formatos fijos complejos.
- Lectura/escritura a archivos.
- Soporte para formato libre (`*`), incluyendo textos con espacios.
- Validación de simetría lectura-escritura.

---

## Ejemplo de uso

```python
from acfortformat.io import read, write

line = write([123, "texto con espacios", 3.14], fmt="*")
print(line)

parsed = read(line, fmt="*")
print(parsed)
```

---

## Autor


**Adolph Cardozo**  
📧 [ajcs.developer@gmail.com](mailto:ajcs.developer@gmail.com)
🔗 [GitHub](https://github.com/acdeveloper-sci)
