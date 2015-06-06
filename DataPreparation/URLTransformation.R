#
# URL Transformation (Base domain extraction with regular expresions)
#

## REFERRING URL (same for LONG URL)

# Obtengo el vector de coincidencias de la regex
r <- regexpr("(?<=//)[[:alnum:].-]+(?=/)", data[["referring_url"]],perl=TRUE,useBytes=TRUE)

# Los casos no-match de la regex (-1) los paso a NA para que sean ignorados
r[r == -1] <- NA

# Creo el vector en el que devolveré el resultado
ref_url <- rep(NA,length(data[["referring_url"]]))

# Los "direct" los dejo tal cual
ref_url[(data[["referring_url"]] == "direct")] <- "direct"

# Los casos match de la regex los inserto en el vector resultado
ref_url[!is.na(r)] <- regmatches(data[["referring_url"]], r)

# Convierto el tipo del resultado de character a factor
data$referring_url <- as.factor(ref_url)


## SECURE CONNECTION (HTTP/HTTPS) LONG URLs (same for REFERRING URL)

# Obtengo el vector de coincidencias de la regex
r <- regexpr("https", data[["long_url"]],perl=TRUE,useBytes=TRUE)

# Creo el vector en el que devolveré el resultado
seg_long_url <- rep(TRUE,length(data[["long_url"]]))

# Introduzco los resultados en el vector resultado
seg_long_url[r == -1] <- FALSE
seg_long_url[is.na(r)] <- NA

## Conexión segura Referring URLs (http o https)

# Obtengo el vector de coincidencias de la regex
r <- regexpr("https", data[["referring_url"]],perl=TRUE,useBytes=TRUE)

# Creo el vector en el que devolveré el resultado
seg_ref_url <- rep(TRUE,length(data[["referring_url"]]))

# Introduzco los resultados en el vector resultado
seg_ref_url[r == -1] <- FALSE
seg_ref_url[is.na(r)] <- NA
