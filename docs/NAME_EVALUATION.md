# Evaluación de ALSdownloadeR

22 septiembre 2026. Evaluación concluida; el usuario aprobó después el cambio. El nombre ALSdownloadeR ya está aplicado en el código local y la configuración de publicación. No se han cambiado servicios publicados.

`ALSdownloadeR` cumple la sintaxis del campo Package de R: letras ASCII, inicio con letra y sin punto final. La aceptación del nombre en un nuevo envío sigue correspondiendo a CRAN. Los dos nombres solo difieren en mayúsculas; no deben distribuirse como dos paquetes independientes, por la portabilidad en sistemas de archivos sin distinción de mayúsculas.

Es viable conservar el repositorio `Cesarito2021/als_downloader`, su historial y sus enlaces. No es necesario borrar ni descargar y reconstruir GitHub. El identificador R es independiente del nombre del repositorio.

El cambio completo necesita actualizar DESCRIPTION, llamadas al namespace, system.file(), workers en segundo plano, plantillas Rmd, tests, instalación y ejemplos, citas, ayudas, artefactos del manual y registro R-universe. Las URLs de páginas y paquetes generadas con el identificador anterior pueden requerir actualización; no se promete redirección automática. Probar instalación limpia del nombre nuevo y migración desde el anterior en Windows y Linux. Para Windows, usar una biblioteca limpia de pruebas, evitando dos instalaciones diferenciadas únicamente por mayúsculas.

Recomendación: si se adopta, realizar una única migración coordinada antes del próximo envío y documentar el cambio respecto al envío cancelado. No modificar publicaciones ni registro R-universe hasta validar el paquete local y confirmar el nombre definitivo. Esta evaluación no requiere eliminar historial.

Fuentes oficiales:
- https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file
- https://docs.r-universe.dev/publish/set-up.html
- https://docs.r-universe.dev/publish/terms.html
