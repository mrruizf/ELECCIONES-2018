library(RSocrata)
library(sqldf)
library(rgdal)
library(dplyr)
#RESULTADOS ELECTORALES 2018 SENADO DE LA REPUBLICA
#https://www.datos.gov.co/en/Resultados-Electorales/RESULTADOS-ELECTORALES-2018-SENADO-DE-LA-REPUBLICA/75f2-fe2s
elecciones<-read.socrata("https://www.datos.gov.co/resource/75f2-fe2s.json")

#LLENAR ESPACIOS EN BLANCO
elecciones$partido[elecciones$candidato=="VOTOS NULOS"]<-"VOTOS NULOS"
elecciones$partido[elecciones$candidato=="VOTOS EN BLANCO"]<-"VOTOS EN BLANCO"
elecciones$partido[elecciones$candidato=="VOTOS NO MARCADOS"]<-"VOTOS NO MARCADOS"
elecciones$candidato[is.na(elecciones$candidato)]<-"SOLO POR EL PARTIDO" ##SOLAMENTE PÁRA CAMBIO RADICAL

elecciones$nmpio[elecciones$nmpio=="MAPIRIPANA" & elecciones$ndepto=="GUAINIA"]<-"BARRANCO MINAS"
#CARGAR INFO ESPACIAL DE DEPTOS Y MPIOS
Departamentos<-readOGR("ELECCIONES-2018/DIVIPOLA/Departamento.shp")
Municipios<-readOGR("ELECCIONES-2018/DIVIPOLA/Municipio.shp")
Depto<-Departamentos@data
Mpio<-Municipios@data
View(Departamentos@data)
View(Municipios@data)

#CAMBIANDO TODO A MAYUSCULAS
Depto <- mutate_all(Depto, funs(toupper))
Mpio <- mutate_all(Mpio, funs(toupper))

#ELIMINANDO TILDES
Depto$DeNombre<-iconv(Depto$DeNombre,from="UTF-8",to="ASCII//TRANSLIT")
Mpio$MpNombre<-iconv(Mpio$MpNombre,from="UTF-8",to="ASCII//TRANSLIT")

#AGREGANDO DATOS FALTANTES A DEPARTAMENTOS
Depto$DeNombre<-gsub("NARINO","NARIÑO",Depto$DeNombre)
Depto$DeNombre<-gsub("VALLE DEL CAUCA","VALLE",Depto$DeNombre)
Depto$DeNombre<-gsub("NORTE DE SANTANDER","NORTE DE SAN",Depto$DeNombre)
Depto$DeNombre<-gsub("SAN ANDRES PROVIDENCIA Y SANTA CATALINA","SAN ANDRES",Depto$DeNombre)
#str(Depto)
Depto <- rbind(Depto, c("70","11","BOGOTA D.C.", "00", "Constitución 86","0","0"))

#AGREGANDO DATOS FALTANTES A MUNICIPIOS
Mpio$MpNombre<-gsub("PUERTO NARINO","PUERTO NARIÑO",Mpio$MpNombre)
Mpio$MpNombre<-gsub("NARINO","NARIÑO",Mpio$MpNombre)
Mpio$MpNombre<-gsub("BOGOTA, D.C.","BOGOTA. D.C.",Mpio$MpNombre)
Mpio$MpNombre<-gsub("CARTAGENA DE INDIAS","CARTAGENA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MONITOS","MOÑITOS",Mpio$MpNombre)
Mpio$MpNombre<-gsub("VISTAHERMOSA","VISTA HERMOSA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MIRITI-PARANA","MIRITI PARANA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("CANASGORDAS","CAÑASGORDAS",Mpio$MpNombre)
Mpio$MpNombre<-gsub("SAN JOSE DE LA MONTANA","SAN JOSE DE LA MONTAÑA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("YONDO","YONDO-CASABE",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PATIA","PATIA (EL BORDO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("SOTARA","SOTARA (PAISPAMBA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("RIO QUITO","RIO QUITO (PAIMADO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("BRICENO","BRICEÑO",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PUERTO NARE","PUERTO NARE-LA MAGDALENA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("TIQUISIO","TIQUISIO (PTO. RICO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("SANTA FE DE ANTIOQUIA","ANTIOQUIA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("DONMATIAS","DON MATIAS",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PENOL","PEÑOL",Mpio$MpNombre)
Mpio$MpNombre<-gsub("ARROYOHONDO","ARROYO HONDO",Mpio$MpNombre)
Mpio$MpNombre<-gsub("AQUITANIA","AQUITANIA (PUEBLOVIEJO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PAZ DE ARIPORO","PAZ DE ARIPORO (MORENO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PIENDAMO TUNIA","PIENDAMO",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MONTANITA","LA MONTAÑITA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PURACE","PURACE (COCONUCO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("BAJO BAUDO","BAJO BAUDO (PIZARRO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("BOJAYA","BOJAYA (BELLAVISTA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MEDIO ATRATO","MEDIO ATRATO (BETE)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MEDIO BAUDO","MEDIO BAUDO (PUERTO MELUK)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("EL PENON","EL PEÑON",Mpio$MpNombre)
Mpio$MpNombre<-gsub("GUICAN DE LA SIERRA","GUICAN",Mpio$MpNombre)
Mpio$MpNombre<-gsub("COTORRA","COTORRA (BONGO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("LA APARTADA","LA APARTADA (FRONTERA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("LA PENA","LA PEÑA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PARATEBUENO","PARATEBUENO (LA NAGUAYA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("ALTO BAUDO","ALTO BAUDO (PIE DE PATO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("BAHIA SOLANO","BAHIA SOLANO (MUTIS)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("EL CANTON DEL SAN PABLO","EL CANTON DEL SAN PABLO (MAN.",Mpio$MpNombre)
Mpio$MpNombre<-gsub("UNION PANAMERICANA","UNION PANAMERICANA (LAS ANIMAS",Mpio$MpNombre)
Mpio$MpNombre<-gsub("BARRANCOMINAS","BARRANCO MINAS",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MORICHAL","MORICHAL (MORICHAL NUEVO)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PANA-PANA","PANA PANA (CAMPO ALEGRE)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("LA ARGENTINA","LA ARGENTINA (PLATA VIEJA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("TESALIA","TESALIA (CARNICERIAS)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("ARIGUANI","ARIGUANI (EL DIFICIL)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("SAN MARTIN","SAN MARTIN DE LOS LLANOS",Mpio$MpNombre)
Mpio$MpNombre<-gsub("EL PINON","EL PIÑON",Mpio$MpNombre)
Mpio$MpNombre<-gsub("PIJINO DEL CARMEN","PIJIÑO DEL CARMEN",Mpio$MpNombre)
Mpio$MpNombre<-gsub("ZONA BANANERA","ZONA BANANERA (SEVILLA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("SAN MARTIN DE LOS LLANOS DE LOBA","SAN MARTIN DE LOBA",Mpio$MpNombre)
Mpio$MpNombre<-gsub("ARBOLEDA","ARBOLEDA (BERRUECOS)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("CUASPUD","CUASPUD (CARLOSAMA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("LOS ANDES","LOS ANDES (SOTOMAYOR)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MAGUI","MAGUI (PAYAN)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("MALLAMA","MALLAMA (PIEDRANCHA)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("ROBERTO PAYAN","ROBERTO PAYAN (SAN JOSE)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("SANTA CRUZ","SANTACRUZ (GUACHAVES)",Mpio$MpNombre)
Mpio$MpNombre<-gsub("OCANA","OCAÑA",Mpio$MpNombre)


Mpio$MpNombre[Mpio$MpNombre=="LOPEZ" & Mpio$MpCodigo=="19418"]<-"LOPEZ (MICAY)"
Mpio$MpNombre[Mpio$MpCodigo=="19517"]<-"PAEZ (BELALCAZAR)"
Mpio$MpNombre[Mpio$MpNombre=="ALBAN" & Mpio$MpCodigo=="52019"]<-"ALBAN (SAN JOSE)"
Mpio$MpNombre[Mpio$MpNombre=="COLON" & Mpio$MpCodigo=="52203"]<-"COLON (GENOVA)"
Mpio$MpNombre[Mpio$MpNombre=="SANTA BARBARA" & Mpio$MpCodigo=="52696"]<-"SANTA BARBARA (ISCUANDE)"
Mpio$MpNombre[Mpio$MpNombre=="ARBOLEDA (BERRUECOS)S" & Mpio$MpCodigo=="54051"]<-"ARBOLEDAS"
Mpio$MpNombre[Mpio$MpNombre=="ARMERO" & Mpio$MpCodigo=="73055"]<-"ARMERO (GUAYABAL)"
Mpio$MpNombre[Mpio$MpNombre=="PACOA" & Mpio$MpCodigo=="97511"]<-"BUENOS AIRES (PACOA)"
Mpio$MpNombre[Mpio$MpNombre=="CALIMA" & Mpio$MpCodigo=="76126"]<-"CALIMA (DARIEN)"
Mpio$MpNombre[Mpio$MpNombre=="COLOSO" & Mpio$MpCodigo=="70204"]<-"COLOSO (RICAURTE)"
Mpio$MpNombre[Mpio$MpNombre=="COVENAS" & Mpio$MpCodigo=="70221"]<-"COVEÑAS"
Mpio$MpNombre[Mpio$MpNombre=="GALERAS" & Mpio$MpCodigo=="70235"]<-"GALERAS (NUEVA GRANADA)"
Mpio$MpNombre[Mpio$MpNombre=="PAPUNAUA" & Mpio$MpCodigo=="97777"]<-"MORICHAL (PAPUNAGUA)"
Mpio$MpNombre[Mpio$MpNombre=="PUERTO CARRENO" & Mpio$MpCodigo=="99001"]<-"PUERTO CARREÑO"
Mpio$MpNombre[Mpio$MpNombre=="LEGUIZAMO" & Mpio$MpCodigo=="86573"]<-"PUERTO LEGUIZAMO"
Mpio$MpNombre[Mpio$MpNombre=="SALDANA" & Mpio$MpCodigo=="73671"]<-"SALDAÑA"
Mpio$MpNombre[Mpio$MpNombre=="SAN JUAN DE BETULIA" & Mpio$MpCodigo=="70702"]<-"SAN JUAN DE BETULIA (BETULIA)"
Mpio$MpNombre[Mpio$MpNombre=="SAN MIGUEL" & Mpio$MpCodigo=="86757"]<-"SAN MIGUEL (LA DORADA)"
Mpio$MpNombre[Mpio$MpNombre=="VALLE DEL GUAMUEZ" & Mpio$MpCodigo=="86865"]<-"VALLE DEL GUAMUEZ (LA HORMIGA)"
#Mpio$MpNombre[Mpio$MpNombre=="PUTUMAYO	VALLE DEL GUAMUEZ (LA HORMIGA)" & Mpio$MpCodigo=="86865"]<-"VALLE DEL GUAMUEZ (LA HORMIGA)"

Mpio$DeCodigo<-substr(Mpio$MpCodigo, 1, 2)
Mpio <- merge(Mpio, Depto, by='DeCodigo')

Mpio <- select(Mpio, -OBJECTID.x, -SHAPE_Leng.x,-SHAPE_Area.x,-OBJECTID.y,-DeArea,-DeNorma,-SHAPE_Leng.y ,-SHAPE_Area.y)

# faltantes <-sqldf("SELECT e.ndepto,e.nmpio,d.MpCodigo FROM elecciones e 
#                   LEFT JOIN Mpio d ON e.nmpio=d.MpNombre 
#                   WHERE d.MpNombre IS NULL and ndepto <>'CONSULADOS'
#                   GROUP BY e.nmpio,d.MpCodigo")
# 
# faltantes_depto <-sqldf("SELECT e.ndepto,d.DeCodigo FROM elecciones e 
#                   LEFT JOIN Depto d ON e.ndepto=d.DeNombre 
#                   WHERE d.DeNombre IS NULL and ndepto <>'CONSULADOS'
#                   GROUP BY e.ndepto,d.DeCodigo")

# elecciones_2018<-sqldf("SELECT e.*,d.DeCodigo FROM elecciones e
#                        LEFT JOIN Depto d
#                        ON e.ndepto=d.DeNombre
#                        WHERE d.DeNombre IS NOT NULL")
# 
# elecciones_2018_mpios<-sqldf("SELECT e.*,d.MpCodigo FROM elecciones e
#                        LEFT JOIN Mpio d
#                        ON e.nmpio=d.MpNombre
#                        WHERE d.MpNombre IS NULL")

#CREANDO TABLAS PARA LA GDB
#TABLA PARTIDO POLITICO
partido<-sqldf("SELECT partido as ParNombre FROM elecciones GROUP BY partido ORDER BY partido")
Idpartido <- seq_len(nrow(partido))
partido<-cbind(Idpartido, partido)

#TABLA CANDIDATO AL SENADO
candidato<-sqldf("SELECT e.candidato as CanNombre ,e.partido as ParNombre, p.Idpartido,sum(e.votos) AS Votos FROM elecciones e
                 INNER JOIN partido p ON p.ParNombre = e.partido 
                 GROUP BY e.candidato,e.partido ORDER BY e.partido")
IdCandidato <- seq_len(nrow(candidato))
candidato<-cbind(IdCandidato, candidato)

#TABLA PUESTO DE VOTACIÓN
puestovotacion<-sqldf("SELECT e.npuesto as PuestoNombre,count(e.mesa) AS CantMesas,sum(e.votos) as TotVotos,e.ndepto AS Depto,e.nmpio AS Mpio FROM elecciones e
                      GROUP BY PuestoNombre,Mpio,Depto ORDER BY Mpio asc")
IdPuestoV <- seq_len(nrow(puestovotacion))
puestovotacion<-cbind(IdPuestoV, puestovotacion)
#PUESTO DE VOTACIÓN CON LOS DATOS DE DIVIPOLA

puestovotacionfinal<-sqldf("SELECT pv.*,m.DeCodigo,m.MpCodigo FROM puestovotacion pv
                           INNER JOIN Mpio m
                           ON (m.MpNombre=pv.Mpio AND m.DeNombre=pv.Depto)")

puestovotacionexterior<-sqldf("SELECT pv.*,m.DeCodigo,m.MpCodigo FROM puestovotacion pv
                           LEFT JOIN Mpio m
                           ON (m.MpNombre=pv.Mpio AND m.DeNombre=pv.Depto)
                           WHERE m.MpCodigo is null")

#RELACIÓN N--N ENTRE PUESTO DE VOTACIÓN Y CANDIDATO
votacion<-sqldf("SELECT pv.IdPuestoV,ec.IdCandidato,sum(ec.votos) AS Votos,ec.mesa FROM puestovotacion pv
                INNER JOIN (SELECT e.*,c.IdCandidato AS IdCandidato FROM elecciones e INNER JOIN candidato c ON (c.CanNombre=e.candidato AND c.ParNombre=e.partido)) ec
                ON (ec.npuesto=pv.PuestoNombre AND ec.nmpio=pv.Mpio AND ec.ndepto=pv.Depto)
                GROUP BY pv.IdPuestoV,ec.IdCandidato,ec.mesa")

votacion$votos<-as.numeric(votacion$Votos)
sum(puestovotacion$TotVotos)-sum(votacion$Votos)
sum(puestovotacionfinal$TotVotos)+sum(puestovotacionexterior$TotVotos)-sum(votacion$Votos)

#################################################################################
##################EXPORTANDO LAS TABLAS A FORMATO CSV O TXT######################
cand<-file("ELECCIONES-2018/candidato.csv", encoding = "UTF-8")
write.csv(candidato,file=cand)
part<-file("ELECCIONES-2018/partido.csv", encoding = "UTF-8")
write.csv(partido,file=part)
write.csv(votacion,"ELECCIONES-2018/votacion.csv")
pvf<-file("ELECCIONES-2018/puestovotacionfinal.csv", encoding = "UTF-8")
write.csv(puestovotacionfinal,file=pvf)
pve<-file("ELECCIONES-2018/puestovotacionexterior.csv", encoding = "UTF-8")
write.csv(puestovotacionexterior,file=pve)
