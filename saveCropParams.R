#'==============================================================================
#'                    saveCropParams: save crop parameters
#'==============================================================================
#'
#' This script aims to create a database of crop parameters of the SOLANUM 
#' model for different potato varieties.
#' 
#'==============================================================================


#-------------------------------------------------------------------------------
# Description of the crop parameters
#-------------------------------------------------------------------------------
CropParamsInfo <- c("wmax : Maximum canopy cover index (fraction)",
                    "tm   : Thermal time at the maximum canopy cover growth rate (C-day)",
                    "te   : Thermal time at the maximum canopy cover value (C-day)",
                    "A    : Maximum harvest index (fraction)",
                    "tu   : Thermal time at maximum tuber partition rate (C-day)",
                    "b    : Thermal time just before the tuber initiation process (C-day)",
                    "RUE  : Average radiation use efficiency (g/MJ)",
                    "DMc  : Dry matter concentration of tubers (fraction)")

CropParamsSymb <- c("wmax", "tm", "te", "A", "tu", "b", "RUE", "DMc")


#-------------------------------------------------------------------------------
# List of crop parameters for different varieties
#-------------------------------------------------------------------------------
CropParamsList <- list(
#                 wmax      tm      te      A      tu       b    RUE    DMc
  "BariAlu72" = c(0.90,  330.0,  870.0,  0.75,  650.0,  190.0,  3.22,  0.20),
  "BariAlu78" = c(0.90,  330.0,  870.0,  0.75,  650.0,  190.0,  3.22,  0.20)
)


#-------------------------------------------------------------------------------
# Metadata of crop parameters for each variety
#-------------------------------------------------------------------------------
n = length(CropParamsList)

for(i in 1:n){
  names(CropParamsList[[i]]) = CropParamsSymb
  attr(CropParamsList[[i]], "VarietyName") = names(CropParamsList)[i]
  attr(CropParamsList[[i]], "CropParamsInfo") = CropParamsInfo
}

save(CropParamsList, file = "CropParamsList.Rdata")





