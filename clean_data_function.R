
clean_microplastics <- function(data) {
  data %>%
    # Extract information from matrix_name
    mutate(
      matrix_type = str_extract(matrix_name, "^[^,]+"), # Produce matrix type column
      size_class = str_extract(matrix_name, ">.*"), # Produce size class column
      size_class_clean = case_when(
        size_class %in% c(">125 to 355 um", ">125 to 500 um", "> 212 um", ">355 um", ">355 to 500 um") ~ "125-500 um",
        size_class %in% c(">500 to 1000 um", ">500 um")                                                ~ "500-1000 um",
        size_class %in% c(">1000 um", ">1 mm")                                                         ~ ">1000 um",
        TRUE ~ NA_character_
      ),
      # Create larger classifications for plastic type
      material_type = case_when(
        plastic_type %in% c("Polyethylene", "Polypropylene", "Polyethylene/polypropylene copolymer",
                            "Polystyrene", "Polystyrene/acrylic copolymer", "Styrene copolymer",
                            "Acrylonitrile butadiene styrene", "Polyester", "Polyethylene terephthalate",
                            "Polyethylene terephthalate/polyurethane", "Acrylic", "Polyacrolein",
                            "Polyvinyl chloride", "Polyvinyl acetate", "Polyvinyl alcohol",
                            "Polyvinyl butyral", "Polyvinyl ether", "Ethylene/vinyl acetate copolymer",
                            "Methyl vinyl ether copolymers", "Polytetrafluoroethylene", "Polycaprolactone",
                            "Polycarbonate", "Polyethylenimine", "Phenolic resin", "Polyether block amide",
                            "Poly(Aryletherketone)", "Polyethylene co-acrylic acid") ~ "Synthetic\nPolymers",
        plastic_type %in% c("Cotton", "Wool", "Cellulosic", "Cellulose acetate",
                            "Anthropogenic (cellulosic)", "Nylon", "Polyurethane") ~ "Textiles",
        plastic_type %in% c("Rubber", "Silicone", "Fluoroelastomer",
                            "Unknown Potentially Rubber") ~ "Elastomers",
        plastic_type %in% c("Paint", "Asphalt", "Stearates, Lubricants, Waxes",
                            "Organic natural material", "Inorganic natural material",
                            "Glass") ~ "Other",
        TRUE ~ "Unknown"
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
        TRUE ~ "Unknown"
      ),
      # Create area column
      area_mm = length_mm * width_mm
    ) %>%
    rename(sample_medium = matrix_type)
}

# select certain columns (keeep sample_medium, remove sample matrix its the same thing
# remove blank water from the dataset
# change tissue = fish

# Potential updated. categories:

library(dplyr)

df <- df %>%
  mutate(category = case_when(
    material %in% c("Polyethylene", "Polypropylene", "Polyethylene/polypropylene copolymer",
                    "Polystyrene", "Polystyrene/acrylic copolymer", "Polyethylene terephthalate",
                    "Acrylonitrile butadiene styrene", "Polycarbonate",
                    "Polyvinyl chloride") ~ "Commodity Plastics",

    material %in% c("Polyester", "Nylon", "Acrylic", "Polyurethane",
                    "Polyether block amide", "Polyethylene terephthalate/polyurethane") ~ "Synthetic Fibers & Textiles",

    material %in% c("Rubber", "Unknown Potentially Rubber", "Asphalt",
                    "Styrene copolymer") ~ "Tire & Road Wear",

    material %in% c("Paint", "Polytetrafluoroethylene", "Silicone", "Fluoroelastomer",
                    "Phenolic resin", "Poly(Aryletherketone)", "Polycaprolactone",
                    "Polyvinyl butyral", "Polyvinyl acetate", "Polyvinyl alcohol",
                    "Ethylene/vinyl acetate copolymer", "Polyethylene co-acrylic acid",
                    "Polyethylenimine", "Methyl vinyl ether copolymers",
                    "Polyvinyl ether", "Polyacrolein") ~ "Coatings & Industrial",

    material %in% c("Cellulose acetate", "Anthropogenic (cellulosic)",
                    "Anthropogenic (protein base)", "Anthropogenic (synthetic)",
                    "Anthropogenic (unknown base)") ~ "Cellulosic & Anthropogenic",

    material %in% c("Cotton", "Wool", "Cellulosic", "Organic natural material",
                    "Inorganic natural material", "Glass") ~ "Natural Materials",

    material %in% c("Stearates, Lubricants, Waxes") ~ "Stearates & Waxes",

    material %in% c("Not Characterized", "Unknown", "Unknown Potentially Rubber") ~ "Uncharacterized",

    TRUE ~ "Other"
  ))
