# Mapping variables for analysis ------------------

comuna_mapping <- data.frame(
  comuna_eta = c("Lo Prado", "La Florida", "Arica", "Los Andes", "Puente Alto", 
                 "Pedro Aguirre Cerda", "Independencia", "Santiago", "Las Condes",
                 "Punta Arenas", "Maipú", "San José de Maipo", "Quilicura", "Paine",
                 "Lota", "Puerto Montt", "San Miguel", "Iquique", "Viña del Mar",
                 "Renca", "Vallenar", "Temuco", "Conchalí", "Coquimbo", "Recoleta",
                 "Concón", "Copiapó", "Pudahuel", "Talcahuano", "Pelarco",
                 "Panguipulli", "Tierra Amarilla", "La Serena", "Linares", "El Tabo",
                 "Angol", "Colina", "Tiltil", "Valparaíso", "San Joaquín",
                 "Constitución", "Vitacura", "El Quisco", "Valdivia", "San Felipe",
                 "La Reina", "Concepción", "Los Ángeles", "Río Claro", "Toltén",
                 "Huechuraba", "Quillota", "La Ligua", "La Cruz", "Parral",
                 "Limache", "Alto Hospicio", "Cabrero", "Algarrobo", "Villarrica",
                 "Chiguayante", "Villa Alemana", "Melipilla", "La Unión", "Puchuncaví",
                 "La Pintana", "Bulnes", "Curarrehue", "Huasco", "Providencia",
                 "Lo Espejo", "Coronel", "Antofagasta", "Calama", "Rancagua",
                 "Yumbel", "Talca", "Lampa", "Curepto", "Ovalle", "Monte Patria",
                 "Santa Juana", "La Granja", "San Bernardo", "Río Bueno", "Villa Alegre",
                 "San Vicente", "Macul", "Santo Domingo", "Cerrillos", "Peñalolén",
                 "Los Lagos", "Quilpué", "Cerro Navia", "Santa Cruz", "Pelluhue",
                 "Buin", "Lo Barnechea", "Chillán", "Tomé", "Yungay", "Doñihue",
                 "Mariquina", "Puerto Varas", "Teno", "El Carmen", "Pichilemu",
                 "Talagante", "Cabildo", "Salamanca", "Quirihue", "Lonquimay",
                 "Chanco", "Pozo Almonte", "Coihaique", "Pirque", "Calera de Tango",
                 "Casablanca", "Aisén", "Santa Bárbara", "Licantén", "Ñuñoa",
                 "El Monte", "Los Vilos", "San Antonio", "Penco", "Quinta Normal",
                 "Laja", "Calera", "Mejillones", "San Carlos", "Quintero",
                 "Traiguén", "El Bosque", "San Gregorio", "San Pedro de la Paz",
                 "Panquehue", "Padre Las Casas", "Arauco", "Porvenir", "Sierra Gorda",
                 "Cauquenes", "Ñiquén", "Combarbalá", "María Pinto", "Victoria",
                 "Longaví", "Olmué", "Mostazal", "Colbún", "Alto del Carmen",
                 "Yerbas Buenas", "Curanilahue", "Taltal", "Peñaflor", "Olivar",
                 "Machalí", "Curicó", "Empedrado", "La Higuera", "Osorno",
                 "Pucón", "Torres del Paine", "San Ignacio", "Coltauco", "Cañete",
                 "Máfil", "Curacautín", "Rauco", "Papudo", "Padre Hurtado",
                 "San Ramón", "Chañaral", "Las Cabras", "Natales", "Molina",
                 "Vicuña", "Graneros", "Collipulli", "San Esteban", "Ignorada",
                 "Hualpén", "Quinchao", "Río Verde", "Caldera", "Pica",
                 "María Elena", "Nacimiento", "Estación Central", "La Cisterna",
                 "Paillaco", "Lago Ranco", "Chillán Viejo", "San Fernando",
                 "Rinconada", "Putre", "Cartagena", "Renaico", "Illapel",
                 "Vilcún", "Lautaro", "Nogales", "Punitaqui", "Llaillay",
                 "Putaendo", "Rengo", "Saavedra", "Diego de Almagro", "Lolol",
                 "Florida", "Portezuelo", "Paiguano", "Ancud", "Isla de Pascua",
                 "Nueva Imperial", "Melipeuco", "Lanco", "Curacaví", "Tocopilla",
                 "Quellón", "Quillón", "Coinco", "Peumo", "Maule", "Tucapel",
                 "San Javier", "Corral", "San Clemente", "Treguaco", "San Rafael",
                 "Carahue", "Santa María", "Loncoche", "Alhué", "Coihueco",
                 "Hualqui", "Quinta de Tilcoco", "Placilla", "Chile Chico",
                 "Palmilla", "Zapallar", "Pinto", "Catemu", "Isla de Maipo",
                 "Canela", "Llanquihue", "Puerto Octay", "Chimbarongo", "Camarones",
                 "Los Álamos", "Andacollo", "Dalcahue", "Gorbea", "Primavera",
                 "San Pedro de Atacama", "Ninhue", "Freirina", "Coelemu",
                 "San Rosendo", "Calle Larga", "Pencahue", "Peralillo", "Purén",
                 "Petorca", "Hualañé", "Tirúa", "Retiro", "Nancagua", "Romeral",
                 "Calbuco", "Contulmo", "Sagrada Familia", "Cochrane", "Freire",
                 "Puyehue", "Litueche", "Cholchol", "Maullín", "Quilleco",
                 "Lebu", "Pitrufquén", "Codegua", "Huara", "Chépica", "Quemchi",
                 "Camiña", "Navidad", "Futrono", "Cunco", "Teodoro Schmidt",
                 "Malloa", "Curaco de Vélez", "Pichidegua", "Hualaihué", "Antuco",
                 "Queilén", "Río Negro", "San Juan de la Costa", "Paredones",
                 "Chonchi", "San Fabián", "Perquenco", "San Nicolás", "Purranque",
                 "Quilaco", "Lago Verde", "Río Hurtado", "Marchihue", "Mulchén",
                 "Castro", "Los Sauces", "Ercilla", "Requínoa", "Galvarino",
                 "Cabo de Hornos", "Lumaco", "Juan Fernández", "Pumanque",
                 "Vichuquén", "Futaleufú", "Cobquecura", "Cisnes", "Palena",
                 "La Estrella", "San Pablo", "Pemuco", "Ránquil", "Colchane",
                 "Chaitén", "Frutillar", "Los Muermos", "Fresia", "Hijuelas",
                 "Padre las Casas", "Requinoa", "Puqueldón", "Ollagüe"),
  
  comuna_com = c("Lo Prado", "La Florida", "Arica", "Los Andes", "Puente Alto",
                 "Pedro Aguirre Cerda", "Independencia", "Santiago", "Las Condes",
                 "Punta Arenas", "Maipu", "San Jose de Maipo", "Quilicura", "Paine",
                 "Lota", "Puerto Montt", "San Miguel", "Iquique", "Vina del Mar",
                 "Renca", "Vallenar", "Temuco", "Conchali", "Coquimbo", "Recoleta",
                 "Concon", "Copiapo", "Pudahuel", "Talcahuano", "Pelarco",
                 "Panguipulli", "Tierra Amarilla", "La Serena", "Linares", "El Tabo",
                 "Angol", "Colina", "Tiltil", "Valparaiso", "San Joaquin",
                 "Constitucion", "Vitacura", "El Quisco", "Valdivia", "San Felipe",
                 "La Reina", "Concepcion", "Los Angeles", "Rio Claro", "Tolten",
                 "Huechuraba", "Quillota", "La Ligua", "La Cruz", "Parral",
                 "Limache", "Alto Hospicio", "Cabrero", "Algarrobo", "Villarrica",
                 "Chiguayante", "Villa Alemana", "Melipilla", "La Union", "Puchuncavi",
                 "La Pintana", "Bulnes", "Curarrehue", "Huasco", "Providencia",
                 "Lo Espejo", "Coronel", "Antofagasta", "Calama", "Rancagua",
                 "Yumbel", "Talca", "Lampa", "Curepto", "Ovalle", "Monte Patria",
                 "Santa Juana", "La Granja", "San Bernardo", "Rio Bueno", "Villa Alegre",
                 "San Vicente", "Macul", "Santo Domingo", "Cerrillos", "Penalolen",
                 "Los Lagos", "Quilpue", "Cerro Navia", "Santa Cruz", "Pelluhue",
                 "Buin", "Lo Barnechea", "Chillan", "Tome", "Yungay", "Donihue",
                 "Mariquina", "Puerto Varas", "Teno", "El Carmen", "Pichilemu",
                 "Talagante", "Cabildo", "Salamanca", "Quirihue", "Lonquimay",
                 "Chanco", "Pozo Almonte", "Coihaique", "Pirque", "Calera de Tango",
                 "Casablanca", "Aisen", "Santa Barbara", "Licanten", "Nunoa",
                 "El Monte", "Los Vilos", "San Antonio", "Penco", "Quinta Normal",
                 "Laja", "Calera", "Mejillones", "San Carlos", "Quintero",
                 "Traiguen", "El Bosque", "San Gregorio", "San Pedro de la Paz",
                 "Panquehue", "Padre las Casas", "Arauco", "Porvenir", "Sierra Gorda",
                 "Cauquenes", "Niquen", "Combarbala", "Maria Pinto", "Victoria",
                 "Longavi", "Olmue", "Mostazal", "Colbun", "Alto del Carmen",
                 "Yerbas Buenas", "Curanilahue", "Taltal", "Penaflor", "Olivar",
                 "Machali", "Curico", "Empedrado", "La Higuera", "Osorno",
                 "Pucon", "Torres del Paine", "San Ignacio", "Coltauco", "Canete",
                 "Mafil", "Curacautin", "Rauco", "Papudo", "Padre Hurtado",
                 "San Ramon", "Chanaral", "Las Cabras", "Natales", "Molina",
                 "Vicuna", "Graneros", "Collipulli", "San Esteban", "Ignorada",
                 "Hualpen", "Quinchao", "Rio Verde", "Caldera", "Pica",
                 "Maria Elena", "Nacimiento", "Estacion Central", "La Cisterna",
                 "Paillaco", "Lago Ranco", "Chillan Viejo", "San Fernando",
                 "Rinconada", "Putre", "Cartagena", "Renaico", "Illapel",
                 "Vilcun", "Lautaro", "Nogales", "Punitaqui", "Llaillay",
                 "Putaendo", "Rengo", "Saavedra", "Diego de Almagro", "Lolol",
                 "Florida", "Portezuelo", "Paiguano", "Ancud", "Isla de Pascua",
                 "Nueva Imperial", "Melipeuco", "Lanco", "Curacavi", "Tocopilla",
                 "Quellon", "Quillon", "Coinco", "Peumo", "Maule", "Tucapel",
                 "San Javier", "Corral", "San Clemente", "Treguaco", "San Rafael",
                 "Carahue", "Santa Maria", "Loncoche", "Alhue", "Coihueco",
                 "Hualqui", "Quinta de Tilcoco", "Placilla", "Chile Chico",
                 "Palmilla", "Zapallar", "Pinto", "Catemu", "Isla de Maipo",
                 "Canela", "Llanquihue", "Puerto Octay", "Chimbarongo", "Camarones",
                 "Los Alamos", "Andacollo", "Dalcahue", "Gorbea", "Primavera",
                 "San Pedro de Atacama", "Ninhue", "Freirina", "Coelemu",
                 "San Rosendo", "Calle Larga", "Pencahue", "Peralillo", "Puren",
                 "Petorca", "Hualane", "Tirua", "Retiro", "Nancagua", "Romeral",
                 "Calbuco", "Contulmo", "Sagrada Familia", "Cochrane", "Freire",
                 "Puyehue", "Litueche", "Cholchol", "Maullin", "Quilleco",
                 "Lebu", "Pitrufquen", "Codegua", "Huara", "Chepica", "Quemchi",
                 "Camina", "Navidad", "Futrono", "Cunco", "Teodoro Schmidt",
                 "Malloa", "Curaco de Velez", "Pichidegua", "Hualaihue", "Antuco",
                 "Queilen", "Rio Negro", "San Juan de la Costa", "Paredones",
                 "Chonchi", "San Fabian", "Perquenco", "San Nicolas", "Purranque",
                 "Quilaco", "Lago Verde", "Rio Hurtado", "Marchihue", "Mulchen",
                 "Castro", "Los Sauces", "Ercilla", "Requinoa", "Galvarino",
                 "Cabo de Hornos", "Lumaco", "Juan Fernandez", "Pumanque",
                 "Vichuquen", "Futaleufu", "Cobquecura", "Cisnes", "Palena",
                 "La Estrella", "San Pablo", "Pemuco", "Ranquil", "Colchane",
                 "Chaiten", "Frutillar", "Los Muermos", "Fresia", "Hijuelas",
                 "Padre las Casas", "Requinoa", "Puqueldon", "Ollague"),
  stringsAsFactors = FALSE
)


food_source_mapping <- data.frame(
  grupo_alimento_sospechoso = c(
    # Eggs
    "Huevos Y Ovoproductos",
    
    # Meat and meat products
    "Carnes Y Productos Carneos (Incluidas Carnes De Aves Y De Caza)",
    
    # Milk and milk products
    "Leche Y Productos Lacteos",
    
    # Seafood
    "Pescados Y Productos De La Pesca",
    
    # Water
    "Bebidas",  # Assuming beverages include water-based drinks
    
    # Ready to eat
    "Comidas Y Platos Preparados",
    
    # Other food
    "Conservas",
    "Productos De Panaderia Y Pasteleria",
    "Productos Elaborados A Partir De Cereales",
    "Helados Y Mezclas Para Helados",
    "Productos De Confiteria",
    "Frutas Y Hortalizas",
    "Productos Grasos",
    "Salsas, Aderezos, Especias Y Condimentos",
    "Caldos, Sopas, Cremas Y Mezclas Deshidratadas",
    "Azucares, Jarabes Y Miel",
    "Estimulantes Y Fruitivos",
    "Alimentos De Uso Infantil",
    
    # Not determined
    "No Identificado"
  ),
  
  source_food = c(
    # Eggs
    "Eggs",
    
    # Meat and meat products
    "Meat and meat products",
    
    # Milk and milk products
    "Milk and milk products",
    
    # Seafood
    "Seafood",
    
    # Water
    "Water",
    
    # Ready to eat
    "Ready to eat",
    
    # Other food
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    "Other food",
    
    # Not determined
    "Not determined"
  ),
  
  stringsAsFactors = FALSE
)

place_source_mapping <- data.frame(
  local_de_elaboracion = c(
    # Place of production and/or consumption
    "Instalacion Elaboracion",
    "Instalación Elaboración", 
    "Instalacion Elaboracion Y Consumo",
    "Instalación Elaboración Y Consumo",
    
    # Place of self-consumption
    "Lugar Autopreparacion",
    "Lugar Autopreparación",
    
    # Place not authorized or only for sale
    "Lugar No Habilitado O Solo Expendio",
    "Lugar No Habilitado O Sólo Expendio",
    
    # Not determined
    "Sin Definir"
  ),
  
  source_place = c(
    # Place of production and/or consumption
    "Place of production and/or consumption",
    "Place of production and/or consumption",
    "Place of production and/or consumption", 
    "Place of production and/or consumption",
    
    # Place of self-consumption
    "Place of self-consumption",
    "Place of self-consumption",
    
    # Place not authorized or only for sale
    "Place not authorized or only for sale",
    "Place not authorized or only for sale",
    
    # Not determined
    "Not determined"
  ),
  
  stringsAsFactors = FALSE
)

diagnosis_mapping <- data.frame(
  diagnostico_agrupado = c(
    # Bacillus cereus
    "Bacillus cereus",
    
    # Bacterium not classified
    "Diagnostico inespecifico",
    
    # Campylobacter spp.
    "Campylobacter spp.",
    
    # Chemical toxins
    "Efecto tóxico de sustancias nocivas ingeridas como alimentos",
    "Intoxicación por plaguicidas",
    
    # Clostridium spp.
    "Clostridium perfringens",
    "Clostridium difficile",
    "Botulismo",
    
    # Escherichia coli
    "Escherichia coli",
    
    # Listeria spp.
    "Listeriosis",
    
    # Not determined
    #"Diagnostico inespecifico",  # This will be handled separately
    
    # Parasites
    "Triquinosis",
    "Giardiasis",
    "Disentería amebiana",
    "Difilobotriosis",
    
    # Salmonella spp.
    "Salmonella spp.",
    "Fiebre tifoidea",
    "Fiebre paratifoidea",
    
    # Seafood toxins
    "Envenenamiento escombroideo por pescado",
    "Otros envenenamientos por alimentos marinos",
    "Envenenamiento ciguatero por pescado",
    
    # Shigella spp.
    "Shigella spp.",
    
    # Staphylococcus aureus
    "Staphylococcus aureus",
    
    # Vibrio spp.
    "Vibrio parahaemolyticus",
    "Cólera epidémico",
    
    # Viruses
    "Norovirus",
    "Rotavirus",
    
    # Yersinia spp.
    "Yersinia spp."
  ),
  
  type_diagnosis = c(
    # Bacillus cereus
    "Bacillus cereus",
    
    # Bacterium not classified
    "Bacterium not classified",
    
    # Campylobacter spp.
    "Campylobacter spp.",
    
    # Chemical toxins
    "Chemical toxins",
    "Chemical toxins",
    
    # Clostridium spp.
    "Clostridium spp.",
    "Clostridium spp.",
    "Clostridium spp.",
    
    # Escherichia coli
    "Escherichia coli",
    
    # Listeria spp.
    "Listeria spp.",
    
    # Not determined
    #"Not determined",
    
    # Parasites
    "Parasites",
    "Parasites",
    "Parasites",
    "Parasites",
    
    # Salmonella spp.
    "Salmonella spp.",
    "Salmonella spp.",
    "Salmonella spp.",
    
    # Seafood toxins
    "Seafood toxins",
    "Seafood toxins",
    "Seafood toxins",
    
    # Shigella spp.
    "Shigella spp.",
    
    # Staphylococcus aureus
    "Staphylococcus aureus",
    
    # Vibrio spp.
    "Vibrio spp.",
    "Vibrio spp.",
    
    # Viruses
    "Viruses",
    "Viruses",
    
    # Yersinia spp.
    "Yersinia spp."
  ),
  
  stringsAsFactors = FALSE
)

group_diagnosis_mapping <- data.frame(
  type_diagnosis = c(
    # Bacteria
    "Bacillus cereus",
    "Bacterium not classified",
    "Campylobacter spp.",
    "Clostridium spp.",
    "Escherichia coli",
    "Listeria spp.",
    "Salmonella spp.",
    "Shigella spp.",
    "Staphylococcus aureus",
    "Vibrio spp.",
    "Yersinia spp.",
    
    # Chemical agents
    "Chemical toxins",
    
    # Not determined
    "Not determined",
    
    # Parasites
    "Parasites",
    
    # Viruses
    "Viruses",
    
    # Seafood toxins (could be chemical or biological)
    "Seafood toxins"
  ),
  
  group_diagnosis = c(
    # Bacteria
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    "Bacteria",
    
    # Chemical agents
    "Chemical agents",
    
    # Not determined
    "Not determined",
    
    # Parasites
    "Parasites",
    
    # Viruses
    "Viruses",
    
    # Seafood toxins (classify as chemical agents)
    "Chemical agents"
  ),
  
  stringsAsFactors = FALSE
)

