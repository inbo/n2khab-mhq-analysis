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

get_lim <- function(data_path, record_ids = NULL) {

  lim <- read_vc(file = "lim", root = data_path) %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment))

  if (is.null(record_ids)) {

    result <- lim

  } else {

    result <- lim %>%
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

  bosconstantie <- read_vc(root = data_path_extra_var, file = "bosconstantie_91E0_sf")

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
                                    record_ids = NULL) {

  vw_forests <- geefInvoervereisten(Versie = "Versie 3",
                                    Habitatgroep = "Bossen en struwelen") %>%
    distinct(Criterium, Indicator,Voorwaarde, Type = TypeVariabele, Eenheid, Invoertype)

  dead_wood <- calc_dead_wood(data_path, data_type = data_type, record_ids) %>%
    filter(variable %in% c("prop_dead_wood", "n_big_dead_ha")) %>%
    mutate(Voorwaarde = ifelse(variable == "prop_dead_wood", "aandeel dood hout",
                               ifelse(variable == "n_big_dead_ha", "aantal exemplaren dik dood hout per ha", NA)),
           Waarde = value,
           ID = record_id) %>%
    inner_join(vw_forests, by = c("Voorwaarde")) %>%
    select(record_id, plot_id, vbi_cycle, Criterium, Indicator ,
           Voorwaarde, Waarde, Type, Eenheid, Invoertype)

  bosconstantie <- read_vc(root = data_path_extra_var, file = "bosconstantie_vbi")

  vw_bosconstantie <- bosconstantie  %>%
    mutate(Indicator = "bosconstantie") %>%
    select(plot_id, Indicator, Waarde = bosconstantie)

  msa <- read_vc(root = data_path_extra_var, file = "msa_vbi")

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

  veglayers <- get_cover_veglayers_fieldmap(data_path, record_ids) %>%
    pivot_wider(names_from = "layer", values_from = cover)

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
    summarise(groeiklasse4 = sum(dbh_mm >= 70 & dbh_mm < 140, na.rm = TRUE) > 0,
              groeiklasse5 = sum(dbh_mm >= 140 & dbh_mm < 500, na.rm = TRUE) > 0,
              groeiklasse6 = sum(dbh_mm >= 500 & dbh_mm < 800, na.rm = TRUE) > 0,
              groeiklasse7 = sum(dbh_mm >= 800, na.rm = TRUE) > 0) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("groeiklasse"), names_to = "growth_class", values_to = "value")

  #groeiklasse3 komt overeen met A2-boom
  trees_a2 <- get_trees_a2(data_path, record_ids)

  growth_class_3 <- trees_a2 %>%
    mutate(growth_class = "groeiklasse3",
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
    mutate(growth_class = "groeiklasse2")

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
    filter(is.na(status_tree) | status_tree == "alive") %>%
    group_by(record_id, plot_id, vbi_cycle) %>%
    summarise(groeiklasse4 = sum(dbh_mm >= 70 & dbh_mm < 140, na.rm = TRUE) > 0,
              groeiklasse5 = sum(dbh_mm >= 140 & dbh_mm < 500, na.rm = TRUE) > 0,
              groeiklasse6 = sum(dbh_mm >= 500 & dbh_mm < 800, na.rm = TRUE) > 0,
              groeiklasse7 = sum(dbh_mm >= 800, na.rm = TRUE) > 0) %>%
    ungroup() %>%
    pivot_longer(cols = starts_with("groeiklasse"), names_to = "growth_class", values_to = "value") %>%
    mutate(value = as.numeric(value))

  analysis_var_vbi <- get_analysis_var_vbi(data_path, record_ids)

  #groeiklasse3 komt overeen met A2-boom
  #groeiklasse2 = natuurlijke verjonging (boomsoort in kruidlaag)

  growth_class_2_3 <- analysis_var_vbi %>%
    filter(variable %in% c("natural_reg", "stem_density_a2_ha")) %>%
    mutate(growth_class = ifelse(variable == "natural_reg", "groeiklasse2",
                                 ifelse(variable == "stem_density_a2_ha", "groeiklasse3", NA))) %>%
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
### grondvlak
###############################################################################

calc_basal_area <- function(data_path, record_ids = NULL, per_segment = FALSE, unit = "Grondvlak_ha"){

  trees_a3a4 <- get_trees_a3a4(data_path, record_ids)

  if(!per_segment){

      plotsize_adj <- trees_a3a4 %>%
        group_by(plot_id, segment_id) %>%
        summarise(AreaA4_m2_segment = unique(AreaA4_m2),
                  AreaA3_m2_segment = unique(AreaA3_m2)) %>%
        group_by(IDPlots) %>%
        summarise(AreaA4_m2 = sum(AreaA4_m2_segment),
                  AreaA3_m2 = sum(AreaA3_m2_segment))

      treesA3A4 <- treesA3A4 %>%
        mutate(IDSegments = 1) %>%
        select(-AreaA4_m2, -AreaA3_m2) %>%
        left_join(plotSize_adj, by = "IDPlots")
    }

    shoots <- getShootsVBI2(db = db, plotIDs = plotIDs)

    treesA3A4_Vol <-  calculateVolumeAndBasalArea(treesA3A4, shoots, dbExterneData = dbVBIExterneData)

  # } else if(databank == "VBI2"){
  #
  #   treesA3A4 <- getTreesA3A4VBI2(db = db, plotIDs = plotIDs)
  #
  #   if(niveau == "plot"){
  #
  #     plotSize_adj <- treesA3A4 %>%
  #       group_by(IDPlots, IDSegments) %>%
  #       summarise(AreaA4_m2_segment = unique(AreaA4_m2),
  #                 AreaA3_m2_segment = unique(AreaA3_m2)) %>%
  #       group_by(IDPlots) %>%
  #       summarise(AreaA4_m2 = sum(AreaA4_m2_segment),
  #                 AreaA3_m2 = sum(AreaA3_m2_segment))
  #
  #     treesA3A4 <- treesA3A4 %>%
  #       mutate(IDSegments = 1) %>%
  #       select(-AreaA4_m2, -AreaA3_m2) %>%
  #       left_join(plotSize_adj, by = "IDPlots")
  #
  #   }
  #   shoots <- getShootsVBI2(db = db, plotIDs = plotIDs)
  #
  #   treesA3A4_Vol <-  calculateVolumeAndBasalArea(treesA3A4, shoots, dbExterneData = dbVBIExterneData)
  #
  # } else if(databank == "VBI1"){
  #
  #   treesA3A4 <- getTreesA3A4VBI1(db = db, plotIDs = plotIDs)
  #   shoots <- getShootsVBI1(db = db, plotIDs = plotIDs)
  #
  #   treesA3A4_Vol <-  calculateVolumeAndBasalAreaVBI1(treesA3A4, shoots, dbExterneData = dbVBIExterneData)
  #
  # }
  #
  # levendHoutSoort <- treesA3A4_Vol %>%
  #   group_by(IDPlots, IDSegments, NameSc) %>%
  #   summarise(Volume_ha = sum(Volume_ha * (StatusTree == "levend"), na.rm = TRUE),
  #             Grondvlak_ha = sum(BasalArea_ha  * (StatusTree == "levend"), na.rm =TRUE)) %>%
  #   ungroup() %>%
  #   gather(Volume_ha, Grondvlak_ha, key = Eenheid, value = Waarde) %>%
  #   rename(Kenmerk = NameSc) %>%
  #   mutate(TypeKenmerk = "soort_latijn",
  #          Type = "Decimaal getal" ,
  #          Kenmerk = as.character(Kenmerk),
  #          ID = paste(substr(databank, 1, 3), IDPlots, ifelse(databank == "VBI2", "_2",
  #                                                             ifelse(databank == "VBI1", "_1", "")), sep =""),
  #          Vegetatielaag = "boomlaag") %>%
  #   filter(Eenheid == eenheid)

  return(levendHoutSoort)

}

###############################################################################
### dead wood
###############################################################################

calc_dead_wood <- function(data_path,
                               record_ids = NULL,
                               data_type = "mhq") {

  trees_a3a4 <- get_trees_a3a4(data_path, record_ids, data_type)

  plot_weights <- get_plot_weights(data_path, record_ids, data_type)

  if (data_type == "vbi") {

    proportion_dead_wood <- get_analysis_var_vbi(data_path, record_ids) %>%
      filter(variable %in% c("logs_vol", "snags_vol",
                             "alive_vol")) %>%
      left_join(select(plot_weights, plot_id, segment_id, vbi_cycle, segment_weight), by = c("plot_id", "segment_id", "vbi_cycle")) %>%
      group_by(record_id, plot_id, vbi_cycle, variable) %>%
      summarise(value_plot = sum(value * segment_weight)) %>%
      ungroup() %>%
      pivot_wider(names_from = "variable", values_from = "value_plot") %>%
      mutate(prop_dead_wood = ifelse((snags_vol + logs_vol + alive_vol) == 0, NA,
                                    (snags_vol + logs_vol) / (snags_vol + logs_vol + alive_vol) * 100),
             prop_dead_wood_standing = ifelse((snags_vol + alive_vol) == 0, NA,
                                              (snags_vol) / (snags_vol + alive_vol) * 100)) %>%
      select(record_id, plot_id, vbi_cycle, prop_dead_wood, prop_dead_wood_standing) %>%
      pivot_longer(cols = c("prop_dead_wood", "prop_dead_wood_standing"), names_to = "variable",
                   values_to = "value")

  }

  plot_weights <- plot_weights %>%
    distinct(record_id, plot_id, vbi_cycle, area_a3_m2_plot, area_a4_m2_plot)

  big_dead_wood <- trees_a3a4 %>%
    left_join(plot_weights, by = c("record_id", "plot_id", "vbi_cycle")) %>%
    group_by(record_id, plot_id, vbi_cycle, area_a4_m2_plot) %>%
    summarise(n_big_dead = sum((dbh_mm > 400) * (status_tree == "dead"), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(value = n_big_dead * 10000 / area_a4_m2_plot,
           variable = "n_big_dead_ha") %>%
    select(record_id, plot_id, vbi_cycle, variable, value)

  dead_wood_var <- proportion_dead_wood %>%
    bind_rows(big_dead_wood) %>%
    arrange(plot_id, vbi_cycle)

return(dead_wood_var)

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
           sorting = c("id", "type_analysis", "voorwaarde"))

  lsvi_indicator <- lsvi_object$Resultaat_indicator %>%
    mutate(Verschilscore = round(Verschilscore, 4)) %>%
    select(ID, type_analysis = Habitattype, Criterium, Indicator, Belang, Status_indicator, Verschilscore)

  colnames(lsvi_indicator) <- str_to_lower(colnames(lsvi_indicator))

  write_vc(lsvi_indicator, file = str_c("lsvi_indicator", suffix), root = path,
           sorting = c("id", "type_analysis", "indicator"))

  lsvi_criterium <- lsvi_object$Resultaat_criterium %>%
    select(-Versie, -Kwaliteitsniveau) %>%
    rename(type_analysis = Habitattype) %>%
    mutate(Index_min_criterium = round(Index_min_criterium, 4),
           Index_harm_criterium = round(Index_harm_criterium, 4))

  colnames(lsvi_criterium) <- str_to_lower(colnames(lsvi_criterium))

  write_vc(lsvi_criterium,  file = str_c("lsvi_criterium", suffix), root = path,
           sorting = c("id", "type_analysis", "criterium"))

  lsvi_globaal <- lsvi_object$Resultaat_globaal %>%
    select(-Versie, -Kwaliteitsniveau) %>%
    rename(type_analysis = Habitattype) %>%
    mutate(Index_min_min = round(Index_min_min, 4),
           Index_harm_harm = round(Index_harm_harm, 4),
           Index_min_harm = round(Index_min_harm, 4))

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





