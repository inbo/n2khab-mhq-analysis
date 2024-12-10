###############################################################################
### cover veglayers inboveg
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
### cover veglayers fieldmap
###############################################################################

get_cover_veglayers_fieldmap <- function(data_path, record_ids = NULL) {

  veglayers <- read_vc(root = data_path, file = "cover_veglayers")

  if ("date_assessment" %in% colnames(veglayers)) {

    cover_veglayers <- veglayers %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment),
             cover = ifelse(is.na(cover), cover_mean, cover)) %>%
      select(record_id, layer, cover)

  } else if ("vbi_cycle" %in% colnames(veglayers)) {

    cover_veglayers <- veglayers %>%
      mutate(record_id = str_c(plot_id, "_", vbi_cycle)) %>%
      select(record_id, layer, cover = cover_mean)

  }

  if (is.null(record_ids)) {

    result <- cover_veglayers

  } else {

    result <- cover_veglayers %>%
      filter(record_id %in% record_ids)

  }

  return(result)

}

###############################################################################
### cover species inboveg
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
### cover species fieldmap
###############################################################################

get_cover_species_fieldmap <- function(data_path, record_ids = NULL) {

  species <- read_vc(root = data_path, file = "cover_species")

  if ("date_assessment" %in% colnames(species)) {

    species <- species %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment))

  } else if ("vbi_cycle" %in% colnames(species)) {

    species <- species %>%
      mutate(record_id = str_c(plot_id, "_", vbi_cycle))

  }

  if (is.null(record_ids)) {

    result <- species

  } else {

    result <- species %>%
      filter(record_id %in% record_ids)

  }

  return(result)

}

###############################################################################
### structure var inboveg
###############################################################################

get_structure_var <- function(data_path, record_ids = NULL){

  structure_var <- read_vc(root = data_path, file = "structure_mhq_terr") %>%
    select(recording_givid, structure_var, cover, cover_code)

  if (is.null(record_ids)) {

    result <- structure_var

  } else {

    result <- structure_var %>%
      filter(recording_givid %in% record_ids)

  }

  return(result)
}

###############################################################################
### structure var fieldmap
###############################################################################

get_structure_var_fieldmap <- function(data_path, record_ids = NULL){

  structure_var <- read_vc(root = data_path, file = "structure_vars") %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment),
           cover = ifelse(is.na(cover), cover_mean, cover)) %>%
    filter(segment_id == 1 | is.na(segment_id)) %>%
    select(record_id, structure_var, cover)

  if (is.null(record_ids)) {

    result <- structure_var

  } else {

    result <- structure_var %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### structure var 1330_da
###############################################################################

get_structure_1330_da <- function(data_path, plot_ids = NULL){

  structure_var <- read_vc(root = data_path, file = "structure_mhq_1330_da") %>%
    select(plot_id, structure_var, value, date)

  if (is.null(plot_ids)) {

    result <- structure_var

  } else {

    result <- structure_var %>%
      filter(plot_id %in% plot_ids)

  }

  return(result)
}

###############################################################################
### analysis var vbi
###############################################################################

get_analysis_var_vbi <- function(data_path, record_ids = NULL){

  analysis_var <- read_vc(root = data_path, file = "analysis_variable") %>%
    mutate(record_id = str_c(plot_id, "_", vbi_cycle))

  if (is.null(record_ids)) {

    result <- analysis_var

  } else {

    result <- analysis_var %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### bomen in a3 en a4 plot
###############################################################################

get_trees_a3a4 <- function(data_path, record_ids = NULL, data_type = "mhq") {

  if (data_type == "mhq") {

    trees_a3a4 <- read_vc(file = "trees_a3a4", root = data_path) %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment))

  } else if (data_type == "vbi") {

    trees_a3a4 <- read_vc(file = "trees_a3a4", root = data_path) %>%
      mutate(record_id = str_c(plot_id, "_", vbi_cycle))

  }

  if (is.null(record_ids)) {

    result <- trees_a3a4

  } else {

    result <- trees_a3a4 %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### plotweights
###############################################################################

get_plot_weights <- function(data_path, record_ids = NULL, data_type = "mhq") {

  if (data_type == "mhq") {

    plot_weights <- read_vc(file = "plot_weight", root = data_path) %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment))

  } else if (data_type == "vbi") {

    plot_weights <- read_vc(file = "plot_weight", root = data_path) %>%
      mutate(record_id = str_c(plot_id, "_", vbi_cycle))

  }

  if (is.null(record_ids)) {

    result <- plot_weights

  } else {

    result <- plot_weights %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### stand description
###############################################################################

get_stand_description <- function(data_path, record_ids = NULL, data_type = "mhq") {

  if (data_type == "mhq") {

    stand_description <- read_vc(file = "standdescription", root = data_path) %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment))

  } else if (data_type == "vbi") {

    stand_description <- read_vc(file = "stand_description", root = data_path) %>%
      mutate(record_id = str_c(plot_id, "_", vbi_cycle))

  }

  if (is.null(record_ids)) {

    result <- stand_description

  } else {

    result <- stand_description %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### bosconstantie
###############################################################################

get_bosconstantie <- function(data_path_extra_var, record_ids = NULL, data_type = "mhq") {

  if (data_type == "mhq") {

    bosconstantie <- read_vc(file = "bosconstantie_mhq", root = data_path_extra_var) %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment))

  } else if (data_type == "vbi") {

    bosconstantie <- read_vc(file = "bosconstantie_vbi", root = data_path_extra_var) %>%
      mutate(record_id = str_c(plot_id, "_", periode))

  } else if (data_type == "91E0_sf") {

    bosconstantie <- read_vc(file = "bosconstantie_91E0_sf", root = data_path_extra_var) %>%
      mutate(record_id = plot_id)

  }

  if (is.null(record_ids)) {

    result <- bosconstantie

  } else {

    result <- bosconstantie %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### msa
###############################################################################

get_msa <- function(data_path_extra_var, record_ids = NULL, data_type = "mhq") {

  if (data_type == "mhq") {

    msa <- read_vc(file = "msa_mhq", root = data_path_extra_var) %>%
      mutate(record_id = str_c(plot_id, "_", date_assessment))

  } else if (data_type == "vbi") {

    msa <- read_vc(file = "msa_vbi", root = data_path_extra_var) %>%
      mutate(record_id = str_c(plot_id, "_", periode))

  }

  if (is.null(record_ids)) {

    result <- msa

  } else {

    result <- msa %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### bomen in a2 plot
###############################################################################

get_trees_a2 <- function(data_path, record_ids = NULL) {

  trees_a2 <- read_vc(file = "trees_a2", root = data_path) %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment))

  if (is.null(record_ids)) {

    result <- trees_a2

  } else {

    result <- trees_a2 %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### hakhout
###############################################################################

get_shoots <- function(data_path, record_ids = NULL) {

  shoots <- read_vc(file = "shoots", root = data_path) %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment))

  if (is.null(record_ids)) {

    result <- shoots

  } else {

    result <- shoots %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### liggend dood hout
###############################################################################

get_logs <- function(data_path, record_ids = NULL) {

  logs <- read_vc(file = "logs", root = data_path) %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment))

  if (is.null(record_ids)) {

    result <- logs

  } else {

    result <- logs %>%
      filter(record_id %in% record_ids)

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

  header <- read_vc(root = data_path, file = "header_mhq_terr") %>%
    rename(record_id = recording_givid)

  if (is.null(record_ids)) {

    result <- header

  } else {

    result <- header %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### site qualifier informatie
###############################################################################

get_site_qualifier <- function(data_path, record_ids = NULL){

  site_qualifier <- read_vc(root = data_path, file = "site_qualifiers_moneos") %>%
    rename(record_id = recording_givid)

  if (is.null(record_ids)) {

    result <- site_qualifier

  } else {

    result <- site_qualifier %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### habitattype informatie
###############################################################################

get_type_inboveg <- function(data_path, record_ids = NULL){

  classif <- read_vc(root = data_path, file = "classif_mhq_terr") %>%
    rename(record_id = recording_givid) %>%
    mutate(type_cover = as.numeric(type_cover),
           type_observed = ifelse(type_observed == "-9", "gh", type_observed))

  if (is.null(record_ids)) {

    result <- classif

  } else {

    result <- classif %>%
      filter(record_id %in% record_ids)

  }

  return(result)
}

###############################################################################
### voorwaarden grasland en moerashabitats
###############################################################################

get_voorwaarden_gr_bm <- function(data_path_fieldmap, data_path_inboveg,
                                  data_path_extravar, record_ids = NULL) {

  voorwaarde_str <- get_cover_veglayers(data_path_inboveg, record_ids) %>%
    filter(layer_code %in% c("STR")) %>%
    mutate(Voorwaarde = ifelse(layer_code == "STR",
                               "bedekking strooisellaag",
                               NA),
           Indicator = ifelse(layer_code == "STR",
                              "strooisellaag",
                              NA),
           Criterium = "Verstoring") %>%
    select(record_id = recording_givid, Criterium, Indicator, Voorwaarde, Waarde = cover)

  voorwaarde_strooisellaag_fieldmap <- get_cover_veglayers_fieldmap(data_path_fieldmap, record_ids) %>%
    filter(layer %in% c("litter")) %>%
    mutate(Indicator = "strooisellaag",
           Voorwaarde = str_c("bedekking ", Indicator),
           Criterium = "Verstoring",
           ID = record_id) %>%
    select(ID, record_id, Criterium, Indicator,
           Voorwaarde, Waarde = cover)

  voorwaarde_struct <- get_structure_var(data_path_inboveg, record_ids) %>%
    filter(structure_var %in% c("verbossing", "structuurschade")) %>%
    mutate(Voorwaarde = str_c("bedekking ", structure_var),
           Criterium = "Verstoring") %>%
    select(record_id = recording_givid, Criterium, Indicator = structure_var,
           Voorwaarde, Waarde = cover)

  voorwaarde_verbossing_fieldmap <- get_structure_var_fieldmap(data_path = data_path_fieldmap, record_ids) %>%
    filter(structure_var %in% c("shrub_treelayer")) %>%
      mutate(Indicator = "verbossing",
             Voorwaarde = str_c("bedekking ", Indicator),
             Criterium = "Verstoring",
             ID = record_id) %>%
      select(ID, record_id, Criterium, Indicator,
             Voorwaarde, Waarde = cover)

  voorwaarde_microrelief <- get_microrelief(data_path_extravar, record_ids) %>%
    mutate(Voorwaarde = "bedekking microrelief",
           Indicator = "microreliëf",
           Criterium = "Structuur") %>%
    rename(Waarde = microreliëf, record_id = recording_givid)

  header <- get_header(data_path_inboveg, record_ids) %>%
    mutate(ID = str_c(user_reference, "_", vague_date_begin)) %>%
    select(record_id, ID)

  voorwaarden <- voorwaarde_str %>%
    bind_rows(voorwaarde_struct) %>%
    bind_rows(voorwaarde_microrelief) %>%
    left_join(header, by = "record_id") %>%
    bind_rows(voorwaarde_verbossing_fieldmap) %>%
    bind_rows(voorwaarde_strooisellaag_fieldmap) %>%
    mutate(Type = ifelse(str_detect(Voorwaarde, "bedekking"), "Percentage", NA),
           Invoertype = NA,
           Eenheid = ifelse(Type == "Percentage", "%", NA),
           ) %>%
    select(record_id, ID, everything()) %>%
    arrange(record_id, Voorwaarde)

  if (is.null(record_ids)) {

    result <- voorwaarden

  } else {

    result <- voorwaarden %>%
      filter(record_id %in% record_ids)

  }

  return(voorwaarden)
}

###############################################################################
### voorwaarden 91E0_sf
###############################################################################

get_voorwaarden_91E0_sf <- function(data_path,
                                    data_path_extra_var,
                                     record_ids = NULL) {

  vw_91E0_sf <- geefInvoervereisten(Versie = "Versie 3",
                                    Habitattype = "91E0_sf") %>%
    select(Criterium, Indicator,Voorwaarde, Type = TypeVariabele, Eenheid, Invoertype)

  voorwaarden <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("aandeel dood hout", "hoeveelheid dik dood hout",
                                "sleutelsoorten boom- en struiklaag")) %>%
    mutate(structure_var = ifelse(structure_var == "sleutelsoorten boom- en struiklaag",
                                  "sleutelsoorten van de boom- en struiklaag", structure_var),
           Waarde = ifelse(structure_var == "hoeveelheid dik dood hout",
                           as.numeric(cover_code),
                           cover)) %>%
    rename(Indicator = structure_var) %>%
    inner_join(vw_91E0_sf, by = "Indicator") %>%
    select(record_id = recording_givid, Criterium, Indicator ,
           Voorwaarde, Waarde, Type, Eenheid, Invoertype)

  header <- get_header(data_path, record_ids) %>%
    select(record_id, vague_date_begin)

  site_qualifier <- get_site_qualifier(data_path, record_ids) %>%
    left_join(header, by = "record_id") %>%
    mutate(ID = str_c(plot_id, "_", vague_date_begin)) %>%
    select(record_id, ID, plot_id)

  voorwaarden <- voorwaarden %>%
    left_join(site_qualifier, by = "record_id") %>%
    select(-plot_id)

  bosconstantie <- get_bosconstantie(data_path_extra_var, data_type = "91E0_sf")

  vw_bosconstantie <- bosconstantie  %>%
    mutate(Indicator = "bosconstantie") %>%
    select(plot_id, Indicator, Waarde = bosconstantie)

  msa <- read_vc(root = data_path_extra_var, file = "msa_91E0_sf")

  vw_msa <- msa  %>%
    mutate(Indicator = "minimum structuurareaal") %>%
    select(plot_id, Indicator, Waarde = MSA)

  vw_extra <- vw_bosconstantie %>%
    bind_rows(vw_msa) %>%
    left_join(site_qualifier, by = "plot_id") %>%
    inner_join(vw_91E0_sf, by = "Indicator") %>%
    select(ID, record_id, Criterium, Indicator ,
           Voorwaarde, Waarde, Type, Eenheid, Invoertype)

  voorwaarden <- voorwaarden %>%
    bind_rows(vw_extra) %>%
    arrange(ID)


  if (is.null(record_ids)) {

    result <- voorwaarden

  } else {

    result <- voorwaarden %>%
      filter(record_id %in% record_ids)

  }

  return(voorwaarden)
}

###############################################################################
### voorwaarden forests
###############################################################################

get_voorwaarden_fs <- function(data_path,
                                    data_path_extra_var,
                                    data_type = "vbi",
                                    path_vol_parameters = NULL,
                                    data_habitat) {

  record_ids <- data_habitat$record_id_square

  vw_forests <- geefInvoervereisten(Versie = "Versie 3",
                                    Habitatgroep = "Bossen en struwelen") %>%
    distinct(Criterium, Indicator,Voorwaarde, Type = TypeVariabele, Eenheid, Invoertype)

  vw_dead_wood <- calc_dead_wood(data_path, data_type = data_type, path_vol_parameters, record_ids) %>%
    filter(variable %in% c("prop_dead_wood", "n_big_dead_ha")) %>%
    mutate(Voorwaarde = ifelse(variable == "prop_dead_wood", "aandeel dood hout",
                               ifelse(variable == "n_big_dead_ha", "aantal exemplaren dik dood hout per ha", NA)),
           Waarde = value)

  stand_age <- get_stand_description(data_path, data_type = data_type, record_ids) %>%
    filter(variable == "stand_age") %>%
    filter(segment_id == 1) %>%
    select(record_id, stand_age = category)

  bosconstantie <- get_bosconstantie(data_path_extra_var, data_type = data_type, record_ids) %>%
    select(record_id, blk)

  vw_bosconstantie <- stand_age  %>%
    full_join(bosconstantie, by = c("record_id")) %>%
    mutate(blk = ifelse(is.na(blk), "missing", blk),
           stand_age = ifelse(is.na(stand_age), "missing", stand_age)) %>%
    mutate(Voorwaarde = "bosconstantie",
           Waarde = ifelse(blk %in% c("Bos ontstaan voor 1775",
                                      "Bos ontstaan tussen 1775 en 1850") |
                             stand_age %in% c("101 - 120 jaar",
                                              "121 - 140 jaar",
                                              "141 - 160 jaar",
                                              "> 160 jaar"),
                           101,
                           ifelse(blk %in% c("Bos ontstaan tussen 1850 en +/- 1930") |
                                  stand_age %in% c("81 - 100 jaar"), 81,
                                  ifelse(stand_age %in% c("41 - 60 jaar", "61 - 80 jaar", "ongelijkjarig"), 41, 20))),
    ) %>%
    select(record_id, Voorwaarde, Waarde)

  vw_mosaic <- stand_age %>%
    mutate(Voorwaarde = "natuurlijke mozaïekstructuur",
           Waarde = ifelse(stand_age == "ongelijkjarig", 1, 0)) %>%
    select(record_id, Voorwaarde, Waarde)

  data_habitat_type <- data_habitat %>%
    select(record_id = record_id_square, Habitattype)

  vw_msa <- get_msa(data_path_extra_var, data_type = data_type, record_ids) %>%
    select(record_id, type, msa_area_ha) %>%
    left_join(data_habitat_type, by = "record_id", relationship = "many-to-many") %>%
    mutate(type_compare = ifelse(str_sub(Habitattype, 1, 4) == "91E0", Habitattype, str_sub(Habitattype, 1, 4))) %>% #enkel voor 91E0 is msa op subtypeniveau bepaald
    filter(type == type_compare) %>%
    mutate(Voorwaarde = "MSA") %>%
    select(record_id, Voorwaarde, Waarde = msa_area_ha, Habitattype) %>%
    mutate(ID = str_c(record_id, "_", Habitattype))

  voorwaarden <- vw_dead_wood %>%
    bind_rows(vw_bosconstantie) %>%
    bind_rows(vw_mosaic) %>%
    left_join(select(data_habitat, record_id = record_id_square, ID), by = "record_id") %>%
    bind_rows(vw_msa) %>% #bevat al ID want type-specifiek
    inner_join(vw_forests, by = c("Voorwaarde")) %>%
    select(ID, record_id, Criterium, Indicator ,
           Voorwaarde, Waarde, Type, Eenheid, Invoertype) %>%
    arrange(ID) %>%
    mutate(Waarde = ifelse(is.na(Waarde), 0,  Waarde))

  if (is.null(record_ids)) {

    result <- voorwaarden

  } else {

    result <- voorwaarden %>%
      filter(record_id %in% record_ids)

  }

  return(voorwaarden)
}

###############################################################################
### voorwaarden 1330_da
###############################################################################

get_voorwaarden_1330_da <- function(data_path,
                                    plot_ids = NULL) {

  vw_1330_da <- geefInvoervereisten(Versie = "Versie 3",
                                    Habitattype = "1330_da") %>%
    select(Criterium, Indicator,Voorwaarde, Type = TypeVariabele, Eenheid, Invoertype)

  voorwaarden <- get_structure_1330_da(data_path, plot_ids) %>%
    rename(Voorwaarde = structure_var, Waarde = value) %>%
    mutate(Voorwaarde = ifelse(Voorwaarde == "intertidale ruimte thv GHW aanwezig",
                               "intertidale ruimte aanwezig", Voorwaarde)) %>%
    inner_join(vw_1330_da, by = "Voorwaarde") %>%
    mutate(Waarde = ifelse(Waarde == "ja", "1",
                           ifelse(Waarde == "nee", "0", Waarde)),
           Waarde = as.numeric(Waarde)) %>%
    select(plot_id, Criterium, Indicator ,
           Voorwaarde, Waarde, Type, Eenheid, Invoertype)

  site_qualifier <- get_site_qualifier(data_path)

  header <- get_header(data_path, record_ids = site_qualifier$record_id) %>%
    select(record_id, vague_date_begin)

  site_qualifier <- site_qualifier %>%
    left_join(header, by = "record_id") %>%
    mutate(ID = str_c(plot_id, "_", vague_date_begin)) %>%
    select(record_id, ID, plot_id)

  #voorlopig nog maar een meting voorwaarde per plot
  voorwaarden <- voorwaarden %>%
    left_join(site_qualifier, by = "plot_id")

  if (is.null(plot_ids)) {

    result <- voorwaarden

  } else {

    result <- voorwaarden %>%
      filter(plot_id %in% plot_ids)

  }

  return(voorwaarden)
}



###############################################################################
### soorten en kenmerken
###############################################################################

get_soorten_kenmerken <- function(data_path_fieldmap = NULL, data_path_inboveg = NULL,
                                  record_ids = NULL){

if (!is.null(data_path_inboveg)) {

  cover_species <- get_cover_species(data_path_inboveg, record_ids) %>%
    mutate(Vegetatielaag = ifelse(layer_code %in% c("K", "KH", "KL"),
                                  "kruidlaag",
                                  ifelse(layer_code %in% c("B", "BH"),
                                         "boomlaag",
                                         ifelse(layer_code %in% c("S", "SH"),
                                                "struiklaag",
                                                ifelse(layer_code %in% c("MO", "ML", "KML", "VML"),
                                                       "moslaag", NA)
                                         )))) %>%
    rename(record_id = recording_givid)

  header <- get_header(data_path_inboveg, record_ids) %>%
    mutate(ID = str_c(user_reference, "_", vague_date_begin)) %>%
    select(record_id, ID)

  cover_species <- cover_species %>%
    mutate(TypeKenmerk = "Soort_Latijn",
           Type = "Percentage",
           Invoertype = NA,
           Eenheid = "%") %>%
    left_join(header, by = "record_id") %>%
    select(record_id, ID, Vegetatielaag, Kenmerk = name_scientific,
           TypeKenmerk, Waarde = species_cover, Type, Eenheid, Invoertype)

} else {

  cover_species <- NULL

}

  if (!is.null(data_path_fieldmap)) {

    cover_species_fieldmap <- get_cover_species_fieldmap(data_path_fieldmap, record_ids) %>%
      mutate(Vegetatielaag = ifelse(layer == "herblayer",
                                    "kruidlaag",
                                    ifelse(layer == "treelayer",
                                           "boomlaag",
                                           ifelse(layer == "shrublayer",
                                                  "struiklaag",
                                                  ifelse(layer == "mosslayer",
                                                         "moslaag", NA)
                                           ))))

    cover_species_fieldmap <- cover_species_fieldmap %>%
      mutate(TypeKenmerk = "Soort_Latijn",
             Type = "Percentage",
             Invoertype = NA,
             Eenheid = "%",
             ID = record_id) %>%
      select(record_id, ID, Vegetatielaag, Kenmerk = name_sc,
             TypeKenmerk, Waarde = cover_mean, Type, Eenheid, Invoertype)

    cover_species <- cover_species_fieldmap %>%
      bind_rows(cover_species) %>%
      arrange(record_id, Kenmerk)

  }
  if (is.null(record_ids)) {

    result <- cover_species

  } else {

    result <- cover_species %>%
      filter(record_id %in% record_ids)

  }

}

###############################################################################
### voorwaarden duinen
###############################################################################

get_voorwaarden_cd <- function(data_path, record_ids = NULL) {

  header <- get_header(data_path, record_ids) %>%
    mutate(ID = str_c(user_reference, "_", vague_date_begin)) %>%
    select(record_id, ID)

  moslaag <- get_cover_veglayers(data_path, record_ids) %>%
    filter(layer_code %in% c("MO")) %>%
    select(record_id = recording_givid, cover_moslaag = cover)

  open_zand <- get_cover_veglayers(data_path, record_ids) %>%
    filter(layer_code %in% c("NB")) %>%
    rename(record_id = recording_givid)

  helm <- get_cover_species(data_path, record_ids) %>%
    filter(name_scientific == "Ammophila arenaria (L.) Link") %>%
    select(record_id = recording_givid, name_scientific, species_cover)

  voorwaarde_structuur <- open_zand %>%
    left_join(helm, by = "record_id") %>%
    mutate(species_cover = ifelse(is.na(species_cover), 0, species_cover)) %>%
    mutate(Voorwaarde = "fijnmazige afwisseling",
           Indicator = "horizontale structuur",
           Criterium = "Structuur",
           Waarde = ifelse((cover > 0) & (species_cover > 0), 1, 0),
           Type = "Ja/nee")

  voorwaarde_moslaag <- open_zand %>%
    left_join(moslaag, by = "record_id") %>%
    mutate(Waarde = ifelse(cover == 100, 0,
                                      cover_moslaag / (100 - cover) * 100)) %>%
    mutate(Voorwaarde = "bedekking alle mossen",
           Indicator = "(korst)moslaag",
           Criterium = "Structuur",
           Type = "Percentage",
           Eenheid = "%")

  voorwaarde_verstuiving <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("gefixeerd_2120",
                                "sterke verstuiving_2120",
                                "beperkte verstuiving_2120")) %>%
    pivot_wider(names_from = structure_var,
                values_from = cover) %>%
    mutate(gefixeerd_2120 = ifelse(is.na(gefixeerd_2120), 0, gefixeerd_2120),
           `sterke verstuiving_2120` =  ifelse(is.na(`sterke verstuiving_2120`), 0, `sterke verstuiving_2120`),
           `beperkte verstuiving_2120` =  ifelse(is.na(`beperkte verstuiving_2120`), 0, `beperkte verstuiving_2120`)) %>%
    mutate(Voorwaarde = "spontane verstuiving aanwezig",
           Indicator = "dynamiek",
           Criterium = "Structuur",
           Waarde = ifelse(`sterke verstuiving_2120` > 0 | `beperkte verstuiving_2120` >= 30, 1, 0),
           Type = "Ja/nee") %>%
    rename(record_id = recording_givid)

  voorwaarde_rijshout <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("rijshout bij type 2120")) %>%
    mutate(Voorwaarde = "bedekking rijshout",
           Indicator = "rijshout",
           Criterium = "Verstoring",
           Type = "Percentage",
           Eenheid = "%",
           Waarde = cover) %>%
    rename(record_id = recording_givid)

  voorwaarde_struweel <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("ontwikkeling_2160",
                                "pionier_2160", "climax_2160",
                                "degeneratie_2160")) %>%
    group_by(recording_givid) %>%
    summarise(Waarde = n_distinct(structure_var)) %>%
    ungroup() %>%
    mutate(Voorwaarde = "aantal struweelsoorten",
           Indicator = "horizontale structuur",
           Criterium = "Structuur",
           Type = "Geheel getal") %>%
    rename(record_id = recording_givid)

  voorwaarde_open_plek <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("open plek_2160")) %>%
    mutate(Voorwaarde = "open plekken aanwezig",
           Indicator = "horizontale structuur",
           Criterium = "Structuur",
           Type = "Percentage",
           Waarde = cover,
           Eenheid = "%") %>%
    rename(record_id = recording_givid)

  voorwaarde_structuurverst <- get_structure_var(data_path, record_ids) %>%
    filter(structure_var %in% c("structuurverstoringen (puin, …)")) %>%
    mutate(Voorwaarde = "bedekking structuurverstoring",
           Indicator = "structuurverstoring",
           Criterium = "Verstoring",
           Type = "Percentage",
           Waarde = cover,
           Eenheid = "%") %>%
    rename(record_id = recording_givid)

  voorwaarden <- voorwaarde_structuur %>%
    bind_rows(voorwaarde_verstuiving) %>%
    bind_rows(voorwaarde_rijshout) %>%
    bind_rows(voorwaarde_struweel) %>%
    bind_rows(voorwaarde_open_plek) %>%
    bind_rows(voorwaarde_moslaag) %>%
    bind_rows(voorwaarde_structuurverst) %>%
    mutate(Invoertype = NA) %>%
    left_join(header, by = "record_id") %>%
    select(record_id, ID, Criterium, Indicator, Voorwaarde, Waarde,
           Type, Eenheid, Invoertype) %>%
    arrange(record_id, Voorwaarde)

  return(voorwaarden)
}

###############################################################################
### voorwaarden heide en inlandse duinen
###############################################################################

get_voorwaarden_hs_id <- function(data_path_inboveg, data_path_fieldmap, record_ids = NULL) {

  header <- get_header(data_path_inboveg, record_ids) %>%
    mutate(ID = str_c(user_reference, "_", vague_date_begin)) %>%
    select(record_id, ID)

 structure_var1 <- get_structure_var_fieldmap(data_path_fieldmap, record_ids) %>%
   filter(structure_var %in% c("herbs", "brushwood", "shrub_treelayer", "campylopus_introflexus", "lowshrublayer")) %>%
   mutate(Indicator = ifelse(structure_var == "herbs", "vergrassing",
                             ifelse(structure_var == "brushwood", "verruiging",
                                    ifelse(structure_var == "shrub_treelayer", "verbossing",
                                           ifelse(structure_var == "campylopus_introflexus", "invasieve exoten",
                                              ifelse(structure_var == "lowshrublayer", "dwergstruiken",NA))))),
          Criterium = ifelse(Indicator == "dwergstruiken", "Structuur", "Verstoring"),
          Voorwaarde = str_c("bedekking ", Indicator),
          Type = "Percentage",
          Waarde = cover,
          Eenheid = "%",
          Invoertype = NA,
          ID = record_id)

 structure_var2 <- get_structure_var(data_path_inboveg, record_ids) %>%
   filter(structure_var %in% c("verbossing", "vergrassing_4030_2310", "verruiging_4030_2310", "grijs_kronkelsteeltje", "dwergstruiken")) %>%
   mutate(Indicator = ifelse(structure_var == "vergrassing_4030_2310", "vergrassing",
                             ifelse(structure_var == "verruiging_4030_2310", "verruiging",
                                           ifelse(structure_var == "grijs_kronkelsteeltje", "invasieve exoten", structure_var))),
          Criterium = ifelse(Indicator == "dwergstruiken", "Structuur", "Verstoring"),
          Voorwaarde = str_c("bedekking ", Indicator),
          Type = "Percentage",
          Waarde = cover,
          Eenheid = "%",
          Invoertype = NA) %>%
   rename(record_id = recording_givid) %>%
   left_join(header, by = "record_id")

 calluna_fieldmap <- get_structure_var_fieldmap(data_path_fieldmap, record_ids) %>%
   filter(str_detect(structure_var, "calluna")) %>%
   mutate(calluna_phase = ifelse(structure_var == "calluna_phase_pioneer", "calluna_pioniersfase",
                                 ifelse(structure_var == "calluna_phase_climax", "calluna_climaxfase",
                                        ifelse(structure_var == "calluna_phase_devel", "calluna_ontwikkelingsfase",
                                               ifelse(structure_var == "calluna_phase_degen", "calluna_degeneratiefase", NA)))),
          ID = record_id)

 calluna_inboveg <- get_structure_var(data_path_inboveg, record_ids) %>%
   filter(str_detect(structure_var, "calluna")) %>%
   rename(record_id = recording_givid, calluna_phase = structure_var) %>%
   left_join(header, by = "record_id")

 voorwaarden_calluna <- calluna_fieldmap %>%
   bind_rows(calluna_inboveg) %>%
   distinct(ID, record_id, calluna_phase, cover) %>%
   mutate(cover = ifelse(is.na(cover), 0, cover)) %>%
   group_by(ID, record_id) %>%
   summarise("aantal ouderdomsstadia" = sum(cover >= 3), # T = 3%
             "climax- of degeneratiestadium aanwezig" = sum((cover >= 3) * calluna_phase %in% c("calluna_climaxfase", "calluna_degeneratiefase")),
             "aanwezigheid Struikheide" = sum(cover) > 0
   ) %>%
   ungroup() %>%
   pivot_longer(cols = c("aantal ouderdomsstadia", "climax- of degeneratiestadium aanwezig", "aanwezigheid Struikheide"),
                names_to = "Voorwaarde",
                values_to = "Waarde") %>%
   mutate(Indicator = ifelse(Voorwaarde == "aanwezigheid Struikheide", "sleutelsoorten",
                             "ouderdomsstructuur Struikheide"),
          Criterium = ifelse(Voorwaarde == "aanwezigheid Struikheide", "Vegetatie",
                            "Structuur"),
          Type = "Geheel getal",
          Eenheid = NA,
          Invoertype = NA)

 pioneer_fieldmap <- get_structure_var_fieldmap(data_path_fieldmap, record_ids) %>%
   filter(str_detect(structure_var, "pioneer_")) %>%
   mutate(pioneer_phase = ifelse(structure_var == "pioneer_lichenen", "pioniersstadie_korstmossen",
                                 ifelse(structure_var == "pioneer_mos", "pioniersstadie_mossen",
                                        ifelse(structure_var == "pioneer_coryn_aira", "pioniersstadia_buntgras",
                                               ifelse(structure_var == "pioneer_phase_open_soil", "pioniersstadia_open_bodem", NA)))),
          ID = record_id)

 pioneer_inboveg <- get_structure_var(data_path_inboveg, record_ids) %>%
   filter(str_detect(structure_var, "pioniersstad")) %>%
   rename(record_id = recording_givid, pioneer_phase = structure_var) %>%
   left_join(header, by = "record_id")

 voorwaarden_pioneer <- pioneer_fieldmap %>%
   bind_rows(pioneer_inboveg) %>%
   distinct(ID, record_id, pioneer_phase, cover) %>%
   mutate(cover = ifelse(is.na(cover), 0, cover)) %>%
   group_by(ID, record_id) %>%
   summarise("bedekking open vegetaties en kaal zand" = pmin(sum(cover), 100),
             "bedekking open vegetaties" = pmin(sum(cover * (pioneer_phase != "pioniersstadia_open_bodem")), 100),
             "bedekking naakte bodem" = sum(cover * (pioneer_phase == "pioniersstadia_open_bodem"))
   ) %>%
   ungroup() %>%
   pivot_longer(cols = c("bedekking open vegetaties en kaal zand", "bedekking open vegetaties", "bedekking naakte bodem"),
                names_to = "Voorwaarde",
                values_to = "Waarde") %>%
   mutate(Indicator = ifelse(Voorwaarde == "bedekking naakte bodem", "naakte bodem",
                             "horizontale structuur"),
          Criterium = "Structuur",
          Type = "Percentage",
          Eenheid = "%",
          Invoertype = NA)

 structure_var <- structure_var1 %>%
   bind_rows(structure_var2) %>%
   bind_rows(voorwaarden_calluna) %>%
   bind_rows(voorwaarden_pioneer) %>%
   select(-structure_var, -cover) %>%
   select(ID, everything())

  return(structure_var)
}

get_kenmerken_open_zand <- function(data_path_inboveg, data_path_fieldmap, record_ids = NULL) {

  open_zand <- get_voorwaarden_hs_id(data_path_inboveg, data_path_fieldmap, record_ids) %>%
    filter(Indicator == "naakte bodem") %>%
    mutate(Kenmerk = "open zand",
           TypeKenmerk = "studiegroep",
           ) %>%
    select(record_id, ID, Kenmerk, TypeKenmerk, Waarde, Eenheid, Type)

  return(open_zand)
}

###############################################################################
### soorten kenmerken moneos
###############################################################################

get_soorten_kenmerken_moneos <- function(data_path, record_ids = NULL) {

  soorten <- get_soorten_kenmerken(data_path_inboveg = data_path,
                                   record_ids = record_ids) %>%
    select(-ID)

  groeiklassen <- get_structure_var(data_path, record_ids) %>%
    filter(str_detect(structure_var, "groeiklasse")) %>%
    mutate(TypeKenmerk = "studiegroep",
           Type = "Percentage",
           Eenheid = "%") %>%
    select(record_id = recording_givid,Kenmerk = structure_var, TypeKenmerk, Type, Waarde = cover, Eenheid)

  cover_sleutelsoorten <- get_structure_var(data_path, record_ids) %>%
    filter(str_detect(structure_var, "bedekking")) %>%
    mutate(Kenmerk = str_to_sentence(str_remove(structure_var, "bedekking ")),
           TypeKenmerk = "Soort_Latijn",
           Vegetatielaag = "kruidlaag",
           Type = "Percentage",
           Eenheid = "%") %>%
    select(record_id = recording_givid, Vegetatielaag, Kenmerk, TypeKenmerk, Type, Waarde = cover, Eenheid)

  header <- get_header(data_path, record_ids) %>%
    select(record_id, vague_date_begin)

  site_qualifier <- get_site_qualifier(data_path, record_ids) %>%
    left_join(header, by = "record_id") %>%
    mutate(ID = str_c(plot_id, "_", vague_date_begin)) %>%
    select(record_id, ID)

  result <- soorten %>%
    bind_rows(groeiklassen) %>%
    bind_rows(cover_sleutelsoorten) %>%
    left_join(site_qualifier, by = "record_id") %>%
    arrange(ID) %>%
    filter(!is.na(Kenmerk))

  return(result)
}

###############################################################################
### soorten kenmerken forests
###############################################################################

get_soorten_kenmerken_fs <- function(data_path, record_ids = NULL, data_type = "mhq"){

  cover_species <- get_soorten_kenmerken(data_path_fieldmap = data_path, record_ids = NULL)

  if (data_type == "mhq") {

    growth_classes <- calc_growth_classes(data_path, record_ids) %>%
      mutate(ID = record_id)

  } else if (data_type == "vbi") {

    growth_classes <- calc_growth_classes_vbi(data_path, record_ids) %>%
      mutate(ID = record_id)

  }

  trees_a3a4 <- get_trees_a3a4(data_path, record_ids, data_type) %>%
    group_by(record_id) %>%
    summarise(n_trees_a3a4 = n()) %>%
    ungroup()

  veglayers <- get_cover_veglayers_fieldmap(data_path, record_ids) %>%
    pivot_wider(names_from = "layer", values_from = cover) %>%
    left_join(trees_a3a4, by = "record_id") %>%
    mutate(herblayer = ifelse(is.na(herblayer), 0, herblayer),
           shrublayer = ifelse(is.na(shrublayer), 0, shrublayer),
           n_trees_a3a4 = ifelse(is.na(n_trees_a3a4), 0, n_trees_a3a4),
           treelayer = ifelse(is.na(treelayer), ifelse(n_trees_a3a4 > 1, 5, 0), treelayer)) # indien er meer dan 1 boom in a3a4plot dan beschouwen we boomlaag talrijk aanwezig

  if (!"mosslayer" %in% colnames(veglayers)) {

    veglayers <- veglayers %>%
      mutate(mosslayer = 0)

  }

  veglayers <- veglayers %>%
    mutate("kruidlaag (incl. moslaag)" = pmin(100, herblayer + mosslayer)) %>%
    rename(struiklaag = shrublayer, boomlaag = treelayer) %>%
    select(-herblayer, -mosslayer) %>%
    pivot_longer(cols = c("boomlaag", "struiklaag", "kruidlaag (incl. moslaag)"),
                 names_to = "Kenmerk", values_to = "Waarde") %>%
    mutate(TypeKenmerk = "studiegroep",
           Eenheid = "%",
           Type = "Percentage",
           ID = record_id)

  result <- bind_rows(cover_species,
                      growth_classes,
                      veglayers) %>%
        arrange(ID) %>%
        select(ID, record_id, Vegetatielaag, Kenmerk, TypeKenmerk, Waarde, Type, Eenheid, Invoertype)

  if (!is.null(record_ids)) {

    result <- result %>%
      filter(record_id %in% record_ids)

  }

  return(result)

  }

###############################################################################
### groeiklassen
###############################################################################

calc_growth_classes <- function(data_path, record_ids = NULL){

  # groeiklasse 4 tot groeiklasse 6 leiden we af uit A3A4 bomen

    trees_a3a4 <- get_trees_a3a4(data_path, record_ids)

  #voor hakhout bepalen we de groeiklasse op basis van de maximale diameter van de shoots

  shoots <- get_shoots(data_path, record_ids) %>%
    group_by(plot_id, tree_id) %>%
    summarise(dbh_mm_max = max(dbh_mm)) %>%
    ungroup()

  # groeiklassen voor levende bomen (dode bomen rekenen we niet mee)
  growth_class_4_5_6_7 <- trees_a3a4 %>%
    left_join(shoots, by = c("plot_id", "tree_id")) %>%
    mutate(dbh_mm = ifelse(!is.na(coppice_individual) & coppice_individual == "Hakhoutstoof" & !is.na(dbh_mm_max),
                           dbh_mm_max, dbh_mm)) %>%
    filter(is.na(status_tree) | status_tree == "levend") %>%
    group_by(plot_id, date_assessment) %>%
    summarise("groeiklasse 4" = sum(dbh_mm >= 70 & dbh_mm < 140, na.rm = TRUE) > 0,
              "groeiklasse 5" = sum(dbh_mm >= 140 & dbh_mm < 500, na.rm = TRUE) > 0,
              "groeiklasse 6" = sum(dbh_mm >= 500 & dbh_mm < 800, na.rm = TRUE) > 0,
              "groeiklasse 7" = sum(dbh_mm >= 800, na.rm = TRUE) > 0) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("groeiklasse"), names_to = "growth_class", values_to = "value")

  #groeiklasse3 komt overeen met A2-boom
  trees_a2 <- get_trees_a2(data_path, record_ids)

  growth_class_3 <- trees_a2 %>%
    mutate(growth_class = "groeiklasse 3",
           value = TRUE) %>%
    distinct(plot_id, date_assessment, growth_class, value)

  #groeiklasse2 = natuurlijke verjonging (boomsoort in kruidlaag)
  cover_species <- get_cover_species_fieldmap(data_path, record_ids)

  trees_genus <- cover_species %>%
    filter(layer == "treelayer") %>%
    distinct(layer, name_nl, name_sc) %>%
    mutate(genus = word(name_sc, 1)) %>%
    filter(!name_nl %in% c("Klimop", "Maretak", "Wilde Kamperfoelie"))

  growth_class_2 <- cover_species %>%
    mutate(genus = word(name_sc, 1)) %>%
    group_by(plot_id, date_assessment) %>%
    summarise(value = any(genus %in% trees_genus$genus & layer == "herblayer")) %>%
    ungroup() %>%
    mutate(growth_class = "groeiklasse 2")

  growth_classes <- growth_class_4_5_6_7 %>%
    bind_rows(growth_class_2) %>%
    bind_rows(growth_class_3) %>%
    rename(Kenmerk = growth_class, Waarde = value) %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment),
           Waarde = ifelse(Waarde, 1, 0),
           TypeKenmerk = "studiegroep",
           Type = "Ja/nee",
           Eenheid = NA) %>%
    arrange(record_id, Kenmerk)

  if (!is.null(record_ids)) {

    growth_classes <- growth_classes %>%
      filter(record_id %in% record_ids)

  }

  return(growth_classes)

}

###############################################################################
### groeiklassen vbi
###############################################################################

calc_growth_classes_vbi <- function(data_path, record_ids = NULL){

  # groeiklasse 4 tot groeiklasse 6 leiden we af uit A3A4 bomen

  trees_a3a4 <- get_trees_a3a4(data_path, record_ids, data_type = "vbi")

  #hakhout nakijken, maar geen gegevens over loten in analysedb

  # groeiklassen voor levende bomen (dode bomen rekenen we niet mee)
  growth_class_4_5_6_7 <- trees_a3a4 %>%
    filter(is.na(status_tree) | status_tree %in% c("alive", "levend")) %>%
    group_by(record_id, plot_id, vbi_cycle) %>%
    summarise("groeiklasse 4" = sum(dbh_mm >= 70 & dbh_mm < 140, na.rm = TRUE) > 0,
              "groeiklasse 5" = sum(dbh_mm >= 140 & dbh_mm < 500, na.rm = TRUE) > 0,
              "groeiklasse 6" = sum(dbh_mm >= 500 & dbh_mm < 800, na.rm = TRUE) > 0,
              "groeiklasse 7" = sum(dbh_mm >= 800, na.rm = TRUE) > 0) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("groeiklasse"), names_to = "growth_class", values_to = "value") %>%
    mutate(value = as.numeric(value))

  analysis_var_vbi <- get_analysis_var_vbi(data_path, record_ids)

  #groeiklasse3 komt overeen met A2-boom
  #groeiklasse2 = natuurlijke verjonging (boomsoort in kruidlaag)
  #ontbrekende waarde = 0

  growth_class_2_3 <- analysis_var_vbi %>%
    filter(variable %in% c("natural_reg", "stem_density_a2_ha")) %>%
    mutate(growth_class = ifelse(variable == "natural_reg", "groeiklasse 2",
                                 ifelse(variable == "stem_density_a2_ha", "groeiklasse 3", NA))) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    group_by(plot_id, record_id, vbi_cycle, growth_class) %>%
    summarise(value = ifelse(sum(value) > 0, 1, 0)) %>%
    ungroup()

  growth_classes <- growth_class_4_5_6_7 %>%
    bind_rows(growth_class_2_3) %>%
    rename(Kenmerk = growth_class, Waarde = value) %>%
    mutate(TypeKenmerk = "studiegroep",
           Type = "Ja/nee",
           Eenheid = NA) %>%
    arrange(record_id, Kenmerk)

  if (!is.null(record_ids)) {

    growth_classes <- growth_classes %>%
      filter(record_id %in% record_ids)

  }

  return(growth_classes)

}


###############################################################################
### calc basal area
###############################################################################

calc_basalarea_ha <- function(data_path, record_ids = NULL, per_segment = FALSE,
                              data_type = "vbi"){

  trees_a3a4 <- get_trees_a3a4(data_path, record_ids, data_type = data_type) %>%
    mutate(segment_id = ifelse(is.na(segment_id), 1, segment_id))

  plot_weights <- get_plot_weights(data_path, record_ids, data_type = data_type) %>%
    select(record_id, plot_id, segment_id, area_a3_m2, area_a4_m2, area_a3_m2_plot, area_a4_m2_plot)

  if (data_type == "mhq") {

    shoots <- get_shoots(data_path, record_ids) %>%
      select(record_id, tree_id, shoot_id, dbh_mm_shoot = dbh_mm)

    trees_a3a4 <- trees_a3a4 %>%
      left_join(shoots, by = c("record_id", "tree_id")) %>%
      mutate(dbh_mm = ifelse(!is.na(dbh_mm_shoot), dbh_mm_shoot, dbh_mm),
            basalarea_m2 = pi * (dbh_mm / 2 / 1000) ^ 2)

  }

  trees_a3a4_weight <- trees_a3a4 %>%
    filter(status_tree %in% c("alive", "levend")) %>%
    left_join(plot_weights, by = c("plot_id", "segment_id", "record_id"))

  if (per_segment) {

    basal_area_ha <- trees_a3a4_weight %>%
      mutate(basalarea_m2_ha = ifelse(perimeter_cm < 122,
                                      basalarea_m2 * 10000 / area_a3_m2,
                                      basalarea_m2 * 10000 / area_a4_m2)) %>%
      group_by(record_id, plot_id,  segment_id, name_nl, name_sc) %>%
      summarise(basalarea_m2_ha = sum(basalarea_m2_ha)) %>%
      ungroup()

  } else {

    basal_area_ha <- trees_a3a4_weight %>%
      mutate(basalarea_m2_ha = ifelse(perimeter_cm < 122,
                                      basalarea_m2 * 10000 / area_a3_m2_plot,
                                      basalarea_m2 * 10000 / area_a4_m2_plot)) %>%
      group_by(record_id, plot_id, name_nl, name_sc) %>%
      summarise(basalarea_m2_ha = sum(basalarea_m2_ha)) %>%
      ungroup() %>%
      mutate(segment_id = 1)

  }

  basal_area_ha <- basal_area_ha %>%
    rename(Kenmerk = name_sc) %>%
    mutate(TypeKenmerk = "soort_latijn",
           Type = "Decimaal getal",
           ID = record_id,
           Vegetatielaag = "boomlaag",
           Eenheid = "Grondvlak_ha") %>%
    select(ID, record_id, plot_id, segment_id, Vegetatielaag, Kenmerk, TypeKenmerk, Waarde = basalarea_m2_ha, Type, Eenheid)

  return(basal_area_ha)

}

###############################################################################
### dead wood
###############################################################################

calc_dead_wood <- function(data_path,
                           path_vol_parameters = path_vbi,
                               record_ids = NULL,
                               data_type = "mhq") {

  trees_a3a4 <- get_trees_a3a4(data_path, record_ids, data_type)
  plot_weights <- get_plot_weights(data_path, record_ids, data_type)

  if (data_type == "mhq") {

  shoots <- get_shoots(data_path, record_ids)
  logs <- get_logs(data_path, record_ids)

  trees_individual_a3a4_height <- trees_a3a4 %>%
    filter(!is.na(height_m)) %>%
    filter(coppice_individual == "Individuele boom") %>%
    calc_volume_tree(n_input = 2, path_vol_parameters = path_vol_parameters)

  trees_individual_a3a4_noheight <- trees_a3a4 %>%
    filter(is.na(height_m)) %>%
    filter(coppice_individual == "Individuele boom") %>%
    calc_volume_tree(n_input = 1, path_vol_parameters = path_vol_parameters)

  shoots_a3a4_height <- trees_a3a4 %>%
    filter(coppice_individual == "Hakhoutstoof") %>%
    select(-dbh_mm, -perimeter_cm) %>%
    filter(!is.na(height_m)) %>%
    left_join(shoots, by = c("plot_id", "tree_id", "record_id", "date_assessment", "mon_cycle")) %>%
    calc_volume_tree(n_input = 2, path_vol_parameters = path_vol_parameters)

  shoots_a3a4_noheight <- trees_a3a4 %>%
    filter(coppice_individual == "Hakhoutstoof") %>%
    select(-dbh_mm, -perimeter_cm) %>%
    filter(is.na(height_m)) %>%
    left_join(shoots, by = c("plot_id", "tree_id", "record_id", "date_assessment", "mon_cycle")) %>%
    calc_volume_tree(n_input = 1, path_vol_parameters = path_vol_parameters)

  plot_weights <- plot_weights %>%
    distinct(plot_id, date_assessment, area_a4_m2_plot, area_a3_m2_plot)

  trees_all <- trees_individual_a3a4_height %>%
    bind_rows(trees_individual_a3a4_noheight) %>%
    bind_rows(shoots_a3a4_height) %>%
    bind_rows(shoots_a3a4_noheight) %>%
    mutate(volume_m3 = ifelse(status_tree == "Niet intacte boom", height_m * pi * (dbh_mm / 1000) ^ 2, volume_m3)) %>%
    left_join(plot_weights, by = c("plot_id",  "date_assessment"), relationship = "many-to-many") %>%
    mutate(volume_m3_ha = ifelse(perimeter_cm < 122, 10000 * volume_m3 / area_a3_m2_plot, 10000 * volume_m3 / area_a4_m2_plot))

  vol_standing_tot <- trees_all %>%
    group_by(record_id, plot_id, date_assessment, status_tree) %>%
    summarise(vol_tot_m3_ha = sum(volume_m3_ha, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(status_tree = ifelse(status_tree == "levend", "alive_vol",
                           ifelse(status_tree == "dood", "snags_vol", NA)))

  li = 45
  vol_logs <- logs %>%
    mutate(volume_m3_ha = pi ^ 2 / 8 / li * (diameter_cm ^ 2) / cos(angle_degrees * pi/180)) %>%
    group_by(record_id, plot_id, date_assessment) %>%
    summarise(vol_tot_m3_ha = sum(volume_m3_ha, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(status_tree = "logs_vol")

  proportion_dead_wood <- vol_standing_tot %>%
    bind_rows(vol_logs) %>%
    pivot_wider(names_from = status_tree, values_from = vol_tot_m3_ha) %>%
    mutate(snags_vol = ifelse(is.na(snags_vol), 0, snags_vol),
           logs_vol = ifelse(is.na(logs_vol), 0, logs_vol)) %>%
    mutate(prop_dead_wood = round(ifelse((snags_vol + logs_vol + alive_vol) == 0, NA,
                                    (snags_vol + logs_vol) / (snags_vol + logs_vol + alive_vol) * 100), 2),
             prop_dead_wood_standing = round(ifelse((snags_vol + alive_vol) == 0, NA,
                                              (snags_vol) / (snags_vol + alive_vol) * 100), 2)) %>%
      select(record_id, prop_dead_wood, prop_dead_wood_standing) %>%
      pivot_longer(cols = c("prop_dead_wood", "prop_dead_wood_standing"), names_to = "variable",
                   values_to = "value")

  big_dead_wood <- trees_a3a4 %>%
    left_join(plot_weights, by = c("plot_id", "date_assessment")) %>%
    group_by(record_id, area_a4_m2_plot) %>%
    summarise(n_big_dead = sum((dbh_mm > 400) * (status_tree == "dood"), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(value = n_big_dead * 10000 / area_a4_m2_plot,
           variable = "n_big_dead_ha") %>%
    select(record_id, variable, value)

  dead_wood_var <- proportion_dead_wood %>%
    bind_rows(big_dead_wood) %>%
    arrange(record_id)

  }
  if (data_type == "vbi") {

    proportion_dead_wood <- get_analysis_var_vbi(data_path, record_ids) %>%
      filter(variable %in% c("logs_vol", "snags_vol",
                             "alive_vol")) %>%
      left_join(select(plot_weights, record_id, segment_weight), by = c("record_id"), relationship = "many-to-many") %>%
      group_by(record_id, variable) %>%
      summarise(value_plot = sum(value * segment_weight)) %>%
      ungroup() %>%
      pivot_wider(names_from = "variable", values_from = "value_plot") %>%
      mutate(prop_dead_wood = ifelse((snags_vol + logs_vol + alive_vol) == 0, NA,
                                    (snags_vol + logs_vol) / (snags_vol + logs_vol + alive_vol) * 100),
             prop_dead_wood_standing = ifelse((snags_vol + alive_vol) == 0, NA,
                                              (snags_vol) / (snags_vol + alive_vol) * 100)) %>%
      select(record_id,  prop_dead_wood, prop_dead_wood_standing) %>%
      pivot_longer(cols = c("prop_dead_wood", "prop_dead_wood_standing"), names_to = "variable",
                   values_to = "value")

    plot_weights <- plot_weights %>%
      distinct(record_id,  area_a3_m2_plot, area_a4_m2_plot)

    big_dead_wood <- trees_a3a4 %>%
      left_join(plot_weights, by = c("record_id")) %>%
      group_by(record_id, area_a4_m2_plot) %>%
      summarise(n_big_dead = sum((dbh_mm > 400) * (status_tree == "dead"), na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(value = n_big_dead * 10000 / area_a4_m2_plot,
             variable = "n_big_dead_ha") %>%
      select(record_id, variable, value)

    dead_wood_var <- proportion_dead_wood %>%
      bind_rows(big_dead_wood) %>%
      arrange(record_id)

  }

return(dead_wood_var)

}

###############################################################################
### volume trees
###############################################################################

calc_volume_tree <- function(trees, n_input, path_vol_parameters) {

    vol_parameters_1input <- read_vc(root = path_vol_parameters, file = "vol_parameters_1input")

    vol_parameters_2input <- read_vc(root = path_vol_parameters, file = "vol_parameters_2input")

  ### 2 ingangen
  if (n_input == 2) {

    trees_vol <- trees %>%
      left_join(select(vol_parameters_2input, -name_nl), by = "tree_species_id") %>% #code soortnaam identiek voor verschillende periodes, maar soortnaam kan verschillen
      mutate(diameter_cm = dbh_mm / 10) %>%
      mutate(volume_m3 = ifelse(formule_type == 1,
                             yes = a + b * perimeter_cm + c * (perimeter_cm^2) + d * (perimeter_cm^3) + e*height_m + f * height_m * perimeter_cm + g * height_m * (perimeter_cm^2),
                             no =  1/1000 *
                               #spil
                               (exp(1.10597 * log(height_m) + 1.78865 * log(diameter_cm) - 3.07192) -
                                  #Verlies
                                  exp(-4.608923 * log(diameter_cm) + 3.005989 * log(height_m) - 1.3209 * log(height_m) * log(height_m) + 1.605266 * log(diameter_cm) * log(height_m) + 5.410272)))) %>%
      select(-a, -b, -c, -d, -e, -f, -g, -formule_type, -tarief) %>%
      mutate(volume_m3 = pmax(0,volume_m3))

  } else if (n_input == 1) {

    trees_vol <- trees %>%
      left_join(select(vol_parameters_1input, -name_nl), by = "tree_species_id") %>% #code soortnaam identiek voor verschillende periodes, maar soortnaam kan verschillen
      mutate(diameter_cm = dbh_mm / 10) %>%
      mutate(volume_m3 = a + b * perimeter_cm + c * (perimeter_cm^2) + d * (perimeter_cm^3)) %>%
      select(-a, -b, -c, -d,  -tarief) %>%
      mutate(volume_m3 = pmax(0,volume_m3))

  }

  return(trees_vol)
}


################################################################################
### write lsvi results
################################################################################

write_lsvi_results <- function(lsvi_object, path, suffix = "") {

  lsvi_detail <- lsvi_object$Resultaat_detail %>%
  mutate(plot_type = ifelse(is.na(plot_type), "square", plot_type),
         waarde_numeric = round(as.numeric(Waarde), 4),
         Verschilscore = round(Verschilscore, 4)) %>%
  select(id = ID, survey, record_id_square, record_id_circle, type_observed, type_analysis = Habitattype, Criterium, Indicator, Belang, Voorwaarde,
         plot_type, Waarde, waarde_numeric, Referentiewaarde, Status_voorwaarde, TheoretischMaximum, Verschilscore)

  colnames(lsvi_detail) <- str_to_lower(colnames(lsvi_detail))

  write_vc(lsvi_detail, file = str_c("lsvi_detail", suffix), root = path,
           sorting = c("id", "type_analysis", "voorwaarde"), strict = FALSE)

  lsvi_indicator <- lsvi_object$Resultaat_indicator %>%
    mutate(Verschilscore = round(Verschilscore, 4)) %>%
    select(ID, type_analysis = Habitattype, Criterium, Indicator, Belang, Status_indicator, Verschilscore)

  colnames(lsvi_indicator) <- str_to_lower(colnames(lsvi_indicator))

  write_vc(lsvi_indicator, file = str_c("lsvi_indicator", suffix), root = path,
           sorting = c("id", "type_analysis", "indicator"), strict = FALSE)

  lsvi_criterium <- lsvi_object$Resultaat_criterium %>%
    select(-Versie, -Kwaliteitsniveau) %>%
    rename(type_analysis = Habitattype)

  colnames(lsvi_criterium) <- str_to_lower(colnames(lsvi_criterium))

  write_vc(lsvi_criterium,  file = str_c("lsvi_criterium", suffix), root = path,
           sorting = c("id", "type_analysis", "criterium"), strict = FALSE)

  lsvi_globaal <- lsvi_object$Resultaat_globaal %>%
    select(-Versie, -Kwaliteitsniveau) %>%
    rename(type_analysis = Habitattype)

  colnames(lsvi_globaal) <- str_to_lower(colnames(lsvi_globaal))

  write_vc(lsvi_globaal, file = str_c("lsvi_globaal", suffix), root = path, strict = FALSE,
           sorting = c("id", "type_analysis"))
}


##############################################################################
### Functies voor paramaterschatting op basis van resultaten LSVI-rekenmodule
##############################################################################

geefParameters <- function(model, type = "binomial"){

  if (type == "binomial"){

    estimates <- NULL

    for(var in rownames((summary(model))$coefficients)){

      AandeelGunstig <- round(plogis(coef(model)[var]) *100, 2)

      confidence_intervals <- confint(model)

      AandeelGunstig_LLCI <- round(plogis(confidence_intervals[var, "2.5 %"])* 100, 2)
      AandeelGunstig_ULCI <- round(plogis(confidence_intervals[var, "97.5 %"])* 100, 2)

      estimates_temp <- data.frame(varName = as.character(var), AandeelGunstig, AandeelGunstig_LLCI, AandeelGunstig_ULCI, stringsAsFactors = FALSE)
      estimates <- bind_rows(estimates, estimates_temp)
    }

  } else if (type == "gaussian"){

    estimates <- NULL

    for(var in rownames((summary(model))$coefficients)){

      Gemiddelde <- round(coef(model)[var], 4)

      confidence_intervals <- confint(model)

      Gemiddelde_LLCI <- round(confidence_intervals[var, "2.5 %"], 4)
      Gemiddelde_ULCI <- round(confidence_intervals[var, "97.5 %"], 4)

      estimates_temp <- data.frame(varName = as.character(var), Gemiddelde, Gemiddelde_LLCI, Gemiddelde_ULCI, stringsAsFactors = FALSE)
      estimates <- bind_rows(estimates, estimates_temp)

    }
  }

  return(estimates)

}

###########################################################################################################

habitatandeelGunstig <- function(data, stratSBZH = FALSE){

  output <- NULL

  for(habitat in unique(data$Habitattype)){

    for (versie in unique(data$Versie)){

      data_versie <- data %>%
        filter(Versie == versie) %>%
        filter(Habitattype == habitat)

      if (stratSBZH){
        design <- svydesign(id = ~1, weights = ~WeightComb, strata = ~SBZH, data = data_versie)

      } else {
        design <- svydesign(id = ~1, weights = ~WeightComb,  data = data_versie)
      }

      #schatting schaal Vlaanderen

      model_Vlaanderen <- svyglm(formula = Status_habitatvlek ~ 1, design = design, family = "quasibinomial")

      param_Vlaanderen <- geefParameters(model_Vlaanderen)

      output_Vlaanderen <- data_versie %>%
        mutate(TypeResultaat = "Habitattype",
               SBZH = "Binnen & Buiten") %>%
        group_by(TypeResultaat, Versie, Habitattype, SBZH) %>%
        summarise(Habitatsubtype = paste(unique(Habitatsubtype), collapse = "; "),
                  nObs = n(),
                  sumWeightsPlot = sum(PlotWeight)/100,
                  sumWeightStratum = sum(StratumWeight),
                  sumWeightsComb = sum(WeightComb),
                  mean = mean(Status_habitatvlek),
                  weightedMean = weighted.mean(Status_habitatvlek, WeightComb)
        ) %>%
        ungroup() %>%
        bind_cols(param_Vlaanderen)

      output <- bind_rows(output, output_Vlaanderen)

      #schatting per SBZH

      if(n_distinct(data_versie$SBZH) > 1) {

        model_SBZH <- svyglm(formula = Status_habitatvlek ~ 0 + SBZH, design = design, family = "quasibinomial")

        param_SBZH <- geefParameters(model_SBZH)

        output_SBZH <- data_versie %>%
          group_by(Versie, Habitattype, SBZH) %>%
          summarise(TypeResultaat = "SBZH",
                    Habitatsubtype = paste(unique(Habitatsubtype), collapse = "; "),
                    nObs = n(),
                    sumWeightsPlot = sum(PlotWeight)/100,
                    sumWeightStratum = sum(StratumWeight),
                    sumWeightsComb = sum(WeightComb),
                    mean = mean(Status_habitatvlek),
                    weightedMean = weighted.mean(Status_habitatvlek, WeightComb)
          ) %>%
          ungroup() %>%
          arrange(SBZH) %>%
          bind_cols(param_SBZH)

        output <- bind_rows(output, output_SBZH)

      }

      #schatting per Subtype

      if(n_distinct(data_versie$Habitatsubtype) > 1){

        #selecteer subtypen met meer dan 1 observatie
        # data_versie <- data_versie %>%
        #   group_by(Habitatsubtype) %>%
        #   mutate(n = n()) %>%
        #   ungroup() %>%
        #   filter(n > 1) %>%
        #   select(-n)

        model_subt <- svyglm(formula = Status_habitatvlek ~ 0 + Habitatsubtype, design = design, family = "quasibinomial")

        param_subt <- geefParameters(model_subt)

        output_subt <- data_versie %>%
          mutate(SBZH = "Binnen & Buiten") %>%
          group_by(Versie, Habitattype, Habitatsubtype, SBZH) %>%
          summarise(TypeResultaat = "Habitatsubtype",
                    nObs = n(),
                    sumWeightsPlot = sum(PlotWeight)/100,
                    sumWeightStratum = sum(StratumWeight),
                    sumWeightsComb = sum(WeightComb),
                    mean = mean(Status_habitatvlek),
                    weightedMean = weighted.mean(Status_habitatvlek, WeightComb)
          ) %>%
          ungroup() %>%
          arrange(Habitatsubtype) %>%
          bind_cols(param_subt)

        output <- bind_rows(output, output_subt)

      }

    }

  }

  # we geven geen betrouwbaarheidsinterval als n < 5
  output <- output %>%
    mutate(AandeelGunstig_LLCI = ifelse(nObs < 5, NA, AandeelGunstig_LLCI),
           AandeelGunstig_ULCI  = ifelse(nObs < 5, NA, AandeelGunstig_ULCI))

  return(output)

}

###########################################################################################################

habitatGemiddeldeVW <- function(data, stratSBZH = TRUE){

  output <- NULL

  for(habitat in unique(data$Habitattype)){

    for (versie in unique(data$Versie)){

      data_versie <- data %>%
        filter(Versie == versie) %>%
        filter(Habitattype == habitat) %>%
        mutate(Waarde = as.numeric(Waarde)) %>%
        filter(!is.na(Waarde))

      for(vw in unique(data_versie$Voorwaarde)){

        if (stratSBZH){
          design <- svydesign(id = ~1, weights = ~WeightComb, strata = ~SBZH, data = data_versie)

        } else {
          design <- svydesign(id = ~1, weights = ~WeightComb,  data = data_versie)
        }

        #schatting schaal Vlaanderen

        model_Vlaanderen <- svyglm(formula = Waarde ~ 1, design = design, family = "gaussian")

        param_Vlaanderen <- geefParameters(model_Vlaanderen, type = "gaussian")

        output_Vlaanderen <- data_versie %>%
          mutate(SBZH = "Binnen & Buiten") %>%
          group_by(Versie, Habitattype, SBZH, Indicator, Voorwaarde) %>%
          summarise(Habitatsubtype = paste(unique(Habitatsubtype), collapse = "; "),
                    nObs = n(),
                    sumWeightsPlot = sum(PlotWeight)/100,
                    sumWeightStratum = sum(StratumWeight),
                    sumWeightsComb = sum(WeightComb),
                    mean = mean(Waarde),
                    weightedMean = weighted.mean(Waarde, WeightComb)
          ) %>%
          ungroup() %>%
          bind_cols(param_Vlaanderen)

        output <- bind_rows(output, output_Vlaanderen)

        #schatting per SBZH

        if(n_distinct(data_versie$SBZH) > 1) {

          model_SBZH <- svyglm(formula = Waarde ~ 0 + SBZH, design = design, family = "gaussian")

          param_SBZH <- geefParameters(model_SBZH, type ="gaussian")

          output_SBZH <- data_versie %>%
            group_by(Versie, Habitattype, SBZH, Indicator, Voorwaarde) %>%
            summarise(Habitatsubtype = paste(unique(Habitatsubtype), collapse = "; "),
                      nObs = n(),
                      sumWeightsPlot = sum(PlotWeight)/100,
                      sumWeightStratum = sum(StratumWeight),
                      sumWeightsComb = sum(WeightComb),
                      mean = mean(Waarde),
                      weightedMean = weighted.mean(Waarde, WeightComb)
            ) %>%
            ungroup() %>%
            arrange(SBZH) %>%
            bind_cols(param_SBZH)

          output <- bind_rows(output, output_SBZH)

        }

        #schatting per Subtype

        if(n_distinct(data_versie$Habitatsubtype) > 1){

          #selecteer subtypen met meer dan 1 observatie
          # data_versie <- data_versie %>%
          #   group_by(Habitatsubtype) %>%
          #   mutate(n = n()) %>%
          #   ungroup() %>%
          #   filter(n > 1) %>%
          #   select(-n)

          model_subt <- svyglm(formula = Waarde ~ 0 + Habitatsubtype, design = design, family = "gaussian")

          param_subt <- geefParameters(model_subt, type = "gaussian")

          output_subt <- data_versie %>%
            mutate(SBZH = "Binnen & Buiten") %>%
            group_by(Versie, Habitattype, Habitatsubtype, SBZH, Indicator, Voorwaarde) %>%
            summarise( nObs = n(),
                       sumWeightsPlot = sum(PlotWeight)/100,
                       sumWeightStratum = sum(StratumWeight),
                       sumWeightsComb = sum(WeightComb),
                       mean = mean(Waarde),
                       weightedMean = weighted.mean(Waarde, WeightComb)
            ) %>%
            ungroup() %>%
            arrange(Habitatsubtype) %>%
            bind_cols(param_subt)

          output <- bind_rows(output, output_subt)

        }


      }

    }
  }


  # we geven geen betrouwbaarheidsinterval als n < 5
  output <- output %>%
    mutate(Gemiddelde_LLCI = ifelse(nObs < 5, NA, Gemiddelde_LLCI),
           Gemiddelde_ULCI  = ifelse(nObs < 5, NA, Gemiddelde_ULCI))

  return(output)

}
###########################################################################################################

aandeel_gunstig_indicator <- function(data_indicator) {

  status_indicatoren <- NULL

  for (habt in unique(data_indicator$Habitattype)) {

    data_habt <- data_indicator %>%
      filter(Habitattype == habt)

    for (ind in unique(data_habt$Indicator)) {

      data_ind <- data_habt %>%
        filter(Indicator == ind) %>%
        mutate(Status_habitatvlek = ifelse(Status_indicator, 1, 0))

      if (nrow(data_ind) > 1) {

        result_temp <- habitatandeelGunstig(data_ind, stratSBZH = FALSE) %>%
          mutate(Indicator = ind,
                 Schaal = "Vlaanderen")

        status_indicatoren <- bind_rows(status_indicatoren, result_temp)
      }
    }
  }

  return(status_indicatoren)
}





