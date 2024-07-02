###############################################################################
### cover veglayers
###############################################################################

get_cover_veglayers <- function(data_path, record_ids = NULL) {

  veglayers <- read_vc(root = data_path, file = "layers_mhq_terr")

  cover_veglayers <- veglayers %>%
    mutate(cover = as.numeric(
                              ifelse(layer_cover == "0-x-1",
                                     "0.5",
                                     as.character(layer_cover))
                              )) %>%
    select(-layer_cover)

  if (is.null(record_ids)) {

    result <- cover_veglayers

  } else {

    result <- cover_veglayers %>%
      filter(recording_givid %in% record_ids)

  }

  return(result)

}

###############################################################################
### cover species
###############################################################################

get_cover_species <- function(data_path, record_ids = NULL) {

  species <- read_vc(root = data_path, file = "vegetation_mhq_terr")

  if (is.null(record_ids)) {

    result <- species

  } else {

    result <- species %>%
      filter(recording_givid %in% record_ids)

  }

  return(result)

}

###############################################################################
### structure var
###############################################################################

get_structure_var <- function(data_path, record_ids = NULL){

  structure_var <- read_vc(root = data_path, file = "structure_mhq_terr") %>%
    select(recording_givid, structure_var, cover)

  if (is.null(record_ids)) {

    result <- structure_var

  } else {

    result <- structure_var %>%
      filter(recording_givid %in% record_ids)

  }

  return(result)
}

###############################################################################
### microreliëf
###############################################################################

get_microrelief <- function(data_path_extravar, record_ids = NULL){

  microrelief <- read_vc(root = data_path_extravar, file = "microreliëf")

  if (is.null(record_ids)) {

    result <- microrelief

  } else {

    result <- microrelief %>%
      filter(recording_givid %in% record_ids)

  }

  return(result)
}

###############################################################################
### header informatie
###############################################################################

get_header <- function(data_path, record_ids = NULL){

  header <- read_vc(root = data_path, file = "header_mhq_terr")

  if (is.null(record_ids)) {

    result <- header

  } else {

    result <- header %>%
      filter(recording_givid %in% record_ids)

  }

  return(result)
}

###############################################################################
### voorwaarden grasland en moerashabitats
###############################################################################

get_voorwaarden_gr_bm <- function(data_path, data_path_extravar, record_ids = NULL) {

  voorwaarde_str <- get_cover_veglayers(data_path, record_ids) %>%
    filter(layer_code %in% c("STR")) %>%
    mutate(Voorwaarde = ifelse(layer_code == "STR",
                               "bedekking strooisellaag",
                               NA),
           Indicator = ifelse(layer_code == "STR",
                              "strooisellaag",
                              NA),
           Criterium = "Verstoring") %>%
    select(recording_givid, Criterium, Indicator, Voorwaarde, Waarde = cover)

  voorwaarde_struct <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("verbossing", "structuurschade")) %>%
    mutate(Voorwaarde = str_c("bedekking ", structure_var),
           Criterium = "Verstoring") %>%
    select(recording_givid, Criterium, Indicator = structure_var,
           Voorwaarde, Waarde = cover)

  voorwaarde_microrelief <- get_microrelief(data_path_extravar, record_ids) %>%
    mutate(Voorwaarde = "bedekking microrelief",
           Indicator = "microreliëf",
           Criterium = "Structuur") %>%
    rename(Waarde = microreliëf)

  header <- get_header(data_path, record_ids) %>%
    mutate(ID = str_c(user_reference, "_", vague_date_begin)) %>%
    select(recording_givid, ID)

  voorwaarden <- voorwaarde_str %>%
    bind_rows(voorwaarde_struct) %>%
    bind_rows(voorwaarde_microrelief) %>%
    mutate(Type = ifelse(str_detect(Voorwaarde, "bedekking"), "Percentage", NA),
           Invoertype = NA,
           Eenheid = ifelse(Type == "Percentage", "%", NA),
           ) %>%
    left_join(header, by = "recording_givid") %>%
    select(recording_givid, ID, everything()) %>%
    arrange(recording_givid, Voorwaarde)

  if (is.null(record_ids)) {

    result <- voorwaarden

  } else {

    result <- voorwaarden %>%
      filter(recording_givid %in% record_ids)

  }

  return(voorwaarden)
}

###############################################################################
### soorten en kenmerken
###############################################################################

get_soorten_kenmerken <- function(data_path, record_ids = NULL){

  cover_species <- get_cover_species(data_path, record_ids) %>%
    mutate(Vegetatielaag = ifelse(layer_code %in% c("K", "KH", "KL"),
                                  "kruidlaag",
                                  ifelse(layer_code %in% c("B", "BH"),
                                         "boomlaag",
                                         ifelse(layer_code %in% c("S", "SH"),
                                                "struiklaag",
                                                ifelse(layer_code %in% c("MO"),
                                                       "moslaag", NA)
                                         ))))

  header <- get_header(data_path, record_ids) %>%
    mutate(ID = str_c(user_reference, "_", vague_date_begin)) %>%
    select(recording_givid, ID)

  result <- cover_species %>%
    mutate(TypeKenmerk = "Soort_Latijn",
           Type = "Percentage",
           Invoertype = NA,
           Eenheid = "%") %>%
    left_join(header, by = "recording_givid") %>%
    select(recording_givid, ID, Vegetatielaag, Kenmerk = name_scientific,
           TypeKenmerk, Waarde = species_cover, Type, Eenheid, Invoertype) %>%
    arrange(recording_givid, Kenmerk)

  return(result)

}
