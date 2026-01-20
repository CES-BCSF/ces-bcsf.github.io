@echo off

REM * RUTA DE ARCHIVO A AUTOMATIZAR
set SCRIPT_R="C:\mysyncfolders\bcsf.com.ar\BCSF - Grupo CES - Documentos\CicSFE_sp\_Reportes rmd\calendario_publicaciones\upload_github.r"

REM * RUTA DEL EJECUTABLE DE R (Rscript.exe)
set RSCRIPT="C:\Program Files\R\R-4.5.1\bin\Rscript.exe"

REM * Ejecuta Rscript con el script de R. >nul 2>&1 oculta la ventana de la consola.
%RSCRIPT% %SCRIPT_R% 

exit