#' Clean Data Function
#'This function is for cleaning the dataset for this analysis.
#' @param data: The one and only '2020-09-11_microparticledata.xlsx'.
#' @returns cleaned data!

clean_microplastics <- function(data) {
  data %>%
    mutate(
      matrix_type = str_extract(matrix_name, "^[^,]+"), # Produce matrix type column
      size_class = str_extract(matrix_name, ">.*"), # Produce size class column
      size_class_clean = case_when(
        size_class %in% c(">125 to 355 um", ">125 to 500 um", "> 212 um", ">355 um", ">355 to 500 um") ~ "125-500 um",
        size_class %in% c(">500 to 1000 um", ">500 um")                                                ~ "500-1000 um",
        size_class %in% c(">1000 um", ">1 mm")                                                         ~ ">1000 um",
        TRUE ~ NA_character_
      ),
      # Larger classification of plastic type
      material_type_large = case_when(
        plastic_type %in% c("Polyethylene", "Polypropylene", "Polyethylene/polypropylene copolymer",
                            "Polystyrene", "Polystyrene/acrylic copolymer", "Styrene copolymer",
                            "Acrylonitrile butadiene styrene", "Polyester", "Polyethylene terephthalate",
                            "Polyethylene terephthalate/polyurethane", "Acrylic", "Polyacrolein",
                            "Polyvinyl chloride", "Polyvinyl acetate", "Polyvinyl alcohol",
                            "Polyvinyl butyral", "Polyvinyl ether", "Ethylene/vinyl acetate copolymer",
                            "Methyl vinyl ether copolymers", "Polytetrafluoroethylene", "Polycaprolactone",
                            "Polycarbonate", "Polyethylenimine", "Phenolic resin", "Polyether block amide",
                            "Poly(Aryletherketone)", "Polyethylene co-acrylic acid") ~ "Plastics",
        plastic_type %in% c("Cotton", "Wool", "Cellulosic", "Cellulose acetate",
                            "Anthropogenic (cellulosic)", "Nylon", "Polyurethane") ~ "Textiles",
        plastic_type %in% c("Rubber", "Silicone", "Fluoroelastomer",
                            "Unknown Potentially Rubber") ~ "Elastomers",
        plastic_type %in% c("Paint", "Asphalt", "Stearates, Lubricants, Waxes",
                            "Organic natural material", "Inorganic natural material",
                            "Glass") ~ "Other",
        TRUE ~ "Unidentified"
      ),
      # Refined classificaiton of plastic type
      material_type_refined = case_when(
        plastic_type %in% c(
          "Polyethylene", "Polypropylene", "Polyethylene/polypropylene copolymer",
          "Polystyrene", "Polyethylene terephthalate", "Polyvinyl chloride",
          "Polycarbonate", "Acrylonitrile butadiene styrene", "Polycaprolactone"
        ) ~ "Common Synthetic Plastics",

        plastic_type %in% c(
          "Polyester", "Nylon", "Acrylic", "Polyurethane",
          "Polystyrene/acrylic copolymer"
        ) ~ "Synthetic Fibers & Textiles",

        plastic_type %in% c(
          "Polytetrafluoroethylene", "Polyether block amide", "Poly(Aryletherketone)",
          "Polyvinyl butyral", "Fluoroelastomer", "Ethylene/vinyl acetate copolymer",
          "Polyethylene terephthalate/polyurethane", "Polyethylene co-acrylic acid",
          "Methyl vinyl ether copolymers", "Polyacrolein", "Polyethylenimine",
          "Polyvinyl ether", "Phenolic resin"
        ) ~ "Industrial & Specialty Polymers",

        plastic_type %in% c(
          "Polyvinyl acetate", "Polyvinyl alcohol", "Cellulose acetate",
          "Paint", "Stearates, Lubricants, Waxes", "Silicone", "Asphalt"
        ) ~ "Other",

        plastic_type %in% c(
          "Cotton", "Wool", "Cellulosic", "Organic natural material",
          "Inorganic natural material"
        ) ~ "Natural Materials",

        plastic_type %in% c(
          "Rubber", "Unknown Potentially Rubber"
        ) ~ "Rubber",

        plastic_type %in% c(
          "Anthropogenic (cellulosic)", "Anthropogenic (protein base)",
          "Anthropogenic (synthetic)", "Anthropogenic (unknown base)",
          "Glass", "Not Characterized", "Unknown", "Styrene copolymer"
        ) ~ "Unidentified plastic",

        .default = "Uncategorized"
      ),
      # Create plastic origin
      polymer_origin = case_when(
        plastic_type %in% c("Polyethylene", "Polypropylene", "Polyethylene/polypropylene copolymer",
                            "Polystyrene", "Polystyrene/acrylic copolymer", "Styrene copolymer",
                            "Acrylonitrile butadiene styrene", "Polyethylene terephthalate",
                            "Polyethylene terephthalate/polyurethane", "Acrylic", "Polyacrolein",
                            "Polyvinyl chloride", "Polyvinyl acetate", "Polyvinyl butyral",
                            "Polyvinyl ether", "Ethylene/vinyl acetate copolymer",
                            "Methyl vinyl ether copolymers", "Polytetrafluoroethylene",
                            "Polycarbonate", "Polyethylenimine", "Phenolic resin", "Polyether block amide",
                            "Poly(Aryletherketone)", "Polyethylene co-acrylic acid",
                            "Nylon", "Polyurethane", "Polyester",
                            "Rubber", "Silicone", "Fluoroelastomer") ~ "Synthetic",
        plastic_type %in% c("Cellulose acetate", "Anthropogenic (cellulosic)",
                            "Polyvinyl alcohol", "Polycaprolactone") ~ "Semi-synthetic",
        plastic_type %in% c("Cotton", "Wool", "Cellulosic",
                            "Organic natural material", "Inorganic natural material") ~ "Natural",
        plastic_type %in% c("Paint", "Asphalt", "Stearates, Lubricants, Waxes", "Glass") ~ "Non-polymer Anthropogenic",
        TRUE ~ "Unidentifed"
      ),
      # Create area column
      area_mm = length_mm * width_mm
    ) %>%
    # Rename matrix type
    rename(sample_medium = matrix_type) %>%
    # Filter out blankwater
    filter(sample_medium != "blankwater") %>%
    # Rename categories
    mutate(sample_medium = case_when(
      sample_medium == "samplewater" ~ "Water",
      sample_medium == "effluent"    ~ "Wastewater",
      sample_medium == "tissue"      ~ "Fish",
      sample_medium == "runoff" ~ "Runoff",
      sample_medium == "sediment" ~ "Sediment",
      .default = sample_medium))
}

