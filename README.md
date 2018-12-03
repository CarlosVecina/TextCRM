# TextCRM
Mejorando nuestro CRM mediante el uso de Text Mining. Parte de mi conferencia en R Hispano 2018.

Ejemplo usando datos abiertos de los comentarios de las APPS bancarias españolas.

# 1- Obtención de datos:

Scrapeado mediante JS montado una API en local desde a la que poder llamar desde R.
https://github.com/facundoolano/google-play-api

# 2- Análisis exploratorias y estudio preliminar de valoraciones.

Primeras gráficas descriptivas en las que vemos las valoraciones de los usuarios que han incluido comentario según banco, su evolución, la posición de cada banco por encima o por debajo de la media...

# 3- Detección de idioma.

Se exploran diferentes librerias de detección de idioma (sistemas probabilísticos, arboles de clasificación, rrnn...), se comparan los resultados y se complementan con el diccionario de términos positivos y negativos creado al comienzo del estudio para apartados posteriores.

# 4- Modelado de tópicos.

Primer approach a un modelado no supervisado mediante LDA después de una limpieza de los datos, datos especialmente ruidosos.

# 5- Análisis de sentimiento.

A modo de demostración realizamos un análisis de sentimiento baasado en diccionario de términos y su frecuencia de aparición. Combinamos varios diccionarios open source, comprobamos su efectividad y los retocamos con el diccionario creado al inicio del estudio con términos propios de nuestro ámbito de aplicación.

# 6- Modelos supervisados.

Se intenta llevar a cabo el modelado supervisado por un lado de la valoración (de 1 a 5) y por otro lado de las flags (soporte técnico, comercial, no.dept) a partir de los comentarios.
