#!/usr/bin/env python3
"""
Instalador automático para ACFortFormat:
- Detecta sistema operativo y versión de Python.
- Descarga el archivo .whl correspondiente desde Releases en GitHub.
- Ejecuta pip para instalarlo.
"""

import sys
import platform
import subprocess
import urllib.request
import os
import shutil

GITHUB_REPO = "acdeveloper-sci/acfortformat"
RELEASE_TAG = "v0.1"

def get_python_tag():
    major = sys.version_info.major
    minor = sys.version_info.minor
    return f"cp{major}{minor}"

def get_platform_tag():
    system = platform.system()
    arch = platform.machine().lower()

    if system == "Windows":
        return "win_amd64"
    elif system == "Linux":
        return "linux_x86_64"
    elif system == "Darwin":
        return "macosx_10_9_x86_64"
    else:
        return None

def construct_whl_name():
    py_tag = get_python_tag()
    plat_tag = get_platform_tag()
    if not plat_tag:
        return None
    return f"acfortformat-0.1-{py_tag}-{py_tag}-{plat_tag}.whl"

def already_installed():
    try:
        import acfortformat
        return True
    except ImportError:
        return False

def download_file(url, dest):
    with urllib.request.urlopen(url) as response:
        with open(dest, "wb") as out_file:
            shutil.copyfileobj(response, out_file)

def main():
    print("== Instalador de ACFortFormat ==")

    if already_installed():
        print("[OK] Ya está instalado 'acfortformat'. No se requiere acción.")
        return

    whl_name = construct_whl_name()
    if not whl_name:
        print("[ERROR] Sistema operativo no soportado para instalación automática.")
        return

    url = f"https://github.com/{GITHUB_REPO}/releases/download/{RELEASE_TAG}/{whl_name}"
    local_file = os.path.join(os.getcwd(), whl_name)

    print(f"Detectado Python: {get_python_tag()} en {platform.system()}")
    print(f"Descargando: {url}")
    try:
        download_file(url, local_file)
    except Exception as e:
        print(f"[ERROR] Error al descargar el archivo: {e}")
        return

    print("Instalando paquete con pip...")
    try:
        subprocess.check_call([sys.executable, "-m", "pip", "install", local_file])
        print("[OK] Instalación completada con éxito.")
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] Error al instalar el paquete: {e}")
    finally:
        if os.path.exists(local_file):
            os.remove(local_file)

    try:
        os.remove(__file__)
    except Exception as e:
        print(f"{e}")


if __name__ == "__main__":
    main()
