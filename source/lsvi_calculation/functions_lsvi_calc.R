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

  cover_veglayers <- veglayers %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment)) %>%
    select(record_id, layer, cover)

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

  species <- read_vc(root = data_path, file = "cover_species") %>%
    mutate(record_id = str_c(plot_id, "_", date_assessment))

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
### soorten en kenmerken
###############################################################################

get_soorten_kenmerken <- function(data_path_fieldmap = NULL, data_path_inboveg,
                                  record_ids = NULL){

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
    mutate(gefixeerd_2120 = ifelse(is.na(gefixeerd_2120), 0, gefixeerd_2120)) %>%
    mutate(Voorwaarde = "spontane verstuiving aanwezig",
           Indicator = "dynamiek",
           Criterium = "Structuur",
           Waarde = ifelse(gefixeerd_2120 < 100, 1, 0),
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
### voorwaarden duinen
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

  write_vc(lsvi_globaal, file = str_c("lsvi_globaal", suffix), root = path,
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





