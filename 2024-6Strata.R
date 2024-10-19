#==================================================================================================
#Project Name: BASIS SALMON - User Defined Region for BASIS Survey - Juvenile Chum Salmon - with Strata
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 7.31.24
#
#Purpose: To generate chum salmon index based on data provided by S. Garcia (ADFG)
#           using the six genetic strata as defined by E. Lee (ADFG GCL)
#
#
#
#==================================================================================================
#NOTES:
# Parameters are estimated by identifying the value of fixed effects that maximizes the marginal likelihood when integrated across random effects.  We approximate this multidimensional integral using the Laplace approximation, as implemented using Template Model Builder (Kristensen et al., 2016).

# TIMINGS:

#==================================================================================================
# devtools::install_github("james-thorson/FishStatsUtils", INSTALL_opts="--no-staged-install")
# devtools::install_github("james-thorson/VAST", INSTALL_opts="--no-staged-install")

require(tidyverse)
require(dplyr)
require(ggthemes)
require(VAST)
require(here)
require(FishStatsUtils)
require(units)
require(ggridges)
require(rnaturalearth)
require(ggspatial)
require(readxl)
require(FishData)


# detach("package:rnaturalearth", unload=TRUE)

# CONTROL ======================================================================

# Do Estimation?
do.est <- TRUE

# Plot Data
plot.data <- TRUE

# Select Species
# species <- "Sockeye Salmon"
species <- "Chum Salmon"

# Version <- "VAST_v12_0_0"

# Abundance or Biomass
type <- c("biom","abund")[2]

# Number of knots (i.e. spatial complexity)
n_x <- 250
# n_x <- 500
# n_x <- 1000

Region <- "User"

fine_scale <- TRUE

treat_nonencounter_as_zero <- TRUE

# ObsModel=c(1,1) #Lognormal and Poisson-Linked Delta
# ObsModel=c(2,1) #Gamma and Poisson-Linked Delta - GOOD USE ME!!!!!
# ObsModel=c(2,0) #Gamma and Standard Delta - Works well, slightly lower R2
# ObsModel=c(2,2) #Gamma and Tweedie - VERY SLOW!!!

# ObsModel=c(1,0) #Lognormal and Standard Delta
# ObsModel=c(1,1) #Lognormal and Poisson-Linked Delta

# Autocorrelation
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0) # Default Fixed Effects - WORKS
# RhoConfig=c("Beta1"=2,"Beta2"=2,"Epsilon1"=2,"Epsilon2"=2) # Random Walk
# RhoConfig=c("Beta1"=4,"Beta2"=4,"Epsilon1"=4,"Epsilon2"=4) # 1st order AR - Beta 1 rho fails
# RhoConfig=c("Beta1"=2,"Beta2"=2,"Epsilon1"=0,"Epsilon2"=0) # Random Walk
# RhoConfig=c("Beta1"=4,"Beta2"=4,"Epsilon1"=0,"Epsilon2"=0) # 1st order AR - WORKS
# RhoConfig=c("Beta1"=0,"Beta2"=4,"Epsilon1"=4,"Epsilon2"=4) # 1st order AR - WORKS


# Explorations =======

# ObsModel=c(1,1) #Lognormal and Poisson-Linked Delta
# RhoConfig=c("Beta1"=4,"Beta2"=4,"Epsilon1"=0,"Epsilon2"=0) 
# Result
# Check bounds for the following parameters:
#   Param starting_value Lower  MLE Upper final_gradient
# 14 Beta_rho2_f           0.01 -0.99 0.99  0.99      -2.521774

# ObsModel=c(2,1) #Lognormal and Poisson-Linked Delta
# RhoConfig=c("Beta1"=4,"Beta2"=4,"Epsilon1"=0,"Epsilon2"=0)
# The following parameters appear to be approaching zero:
#   Param starting_value Lower           MLE Upper final_gradient
# 11 L_beta2_z              1  -Inf -3.602815e-06   Inf    -0.00140595
# Please turn off factor-model variance parameters `L_` that are approaching zero and re-run the model

# ObsModel=c(2,1) #Gamma and Poisson-Linked Delta
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)
# Ran but DHARMA residuals not ideal

# ObsModel=c(1,1) #Lognormal and Poisson-Linked Delta
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)
# The following parameters appear to be approaching zero:
#   Param starting_value Lower          MLE Upper final_gradient
# 47 L_epsilon2_z              1  -Inf 1.978222e-06   Inf   5.193601e-05
# Please turn off factor-model variance parameters `L_` that are approaching zero and re-run the model

# Alternatives ============


# ObsModel=c(1,0) #Lognormal and Standard Delta Model - FAIL
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)
# 
# ObsModel=c(1,1) #Lognormal and Poisson-Linked Delta - FAIL
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)

# ObsModel=c(5,0) #Zero-inflated Negative Binomial and Standard Delta Model - Failed
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)


# ObsModel=c(7,0) #Zero-inflated Poisson - Error in estimation
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)

# ObsModel=c(2,1) #Gamma and Poisson-Linked Delta - RW for years - FAIL
# RhoConfig=c("Beta1"=2,"Beta2"=2,"Epsilon1"=0,"Epsilon2"=0)
# 
# The following parameters appear to be approaching zero:
#   Param starting_value Lower           MLE Upper final_gradient
# 5  L_beta1_z              1  -Inf  9.964962e-06   Inf   0.0009009601
# 10 L_beta2_z              1  -Inf -1.671965e-06   Inf  -0.0009170077
# Please turn off factor-model variance parameters `L_` that are approaching zero and re-run the model

# ObsModel=c(2,1) #Gamma and Poisson-Linked Delta - RW for years - FAIL
# RhoConfig=c("Beta1"=5,"Beta2"=4,"Epsilon1"=0,"Epsilon2"=0)

# Best Parameterization ======================
# ObsModel=c(2,1) #Gamma and Poisson-Linked Delta
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)

# ObsModel=c(2,0) #Gamma and Standard Delta Model
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)



ObsModel=c(10,2) #Tweedie Model
RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)


# Workflow =====================================================================
dir.data <- here("data")

dir.vast <- file.path(here(), "figs", "2024", "Strata", species, type,
                      paste("n_x", n_x, 
                            "ObsModel", ObsModel[1], ObsModel[2],
                            "fine_scale", fine_scale,
                            "RhoConfig", RhoConfig[1], RhoConfig[2], RhoConfig[3], RhoConfig[4]))

dir.create(dir.vast, recursive=TRUE)

dir.output <- here("output")
dir.R <- here("R")
# Directory for User Extrapolation Grid
dir.extrap <- dir.data

# Source FishStatsUtils function that cannot be loaded =========================
# source(file.path(dir.R, "strip_units.R")) #No longer needed

# Copy Extrapolation Grid File to VAST Folder ==================================
# if(n_x==250) {
#   file.copy(from=file.path(dir.extrap,"Kmeans_extrapolation-2500.RData"),
#             to=file.path(dir.vast, "Kmeans_extrapolation-2500.RData"))
# }
# if(n_x==500) {
#   file.copy(from=file.path(dir.extrap,"Kmeans_extrapolation-5000.RData"),
#             to=file.path(dir.vast, "Kmeans_extrapolation-5000.RData"))
# }
# 
# if(n_x==1000) {
#   file.copy(from=file.path(dir.extrap,"Kmeans_extrapolation-10000.RData"),
#             to=file.path(dir.vast, "Kmeans_extrapolation-10000.RData"))
# }

# Read in Compiled Basis Data ==================================================

# 2024 Data from S. Garcia
dat.catch <- read_xlsx(file.path(dir.data,"2024 Data","Sabrina_J_Chum_10.16.xlsx"),
                       sheet="Catch")

str(dat.catch)

dat.event <- read_xlsx(file.path(dir.data,"2024 Data","Sabrina_J_Chum_10.16.xlsx"),
                       sheet="Event")

str(dat.event)


# Select necessary fields
sort(names(dat.event))
sort(names(dat.catch))

# Select Only Surface Tows: EVENT
unique(dat.event$Description)
dat.event %>% group_by(Description) %>% summarize(count=n()) # Majority surface

dat.event.2 <- dat.event %>% dplyr::filter(Description=="Surface")
unique(dat.event.2$Description) #CHECK!

# Select only satisfactory tows ? - TrawlPerformance field unavailable
unique(dat.event.2$Notes)

# Select only juvenile LHS: CATCH
dat.catch %>% group_by(LHSCode) %>% summarize(count=n())
dat.catch.2 <- dat.catch %>% dplyr::filter(LHSCode=="J")
unique(dat.catch.2$LHSCode) #CHECK!

# Check that only chum included
unique(dat.catch.2$CommonName) #CHECK! only Chum salmon

# Join data together
dat.input <- dat.event.2 %>% left_join(dat.catch.2, by=c("SampleYear"="SampleYear",
                                                         "StationID"="StationID"))

dim(dat.input)

dat.inner <- dat.event.2 %>% inner_join(dat.catch.2, by=c("SampleYear"="SampleYear",
                                                         "StationID"="StationID"))
dim(dat.inner)
# Add zeros

FishData::add_missing_zeros(data_frame=dat.input, unique_sample_ID_colname="HAULJOIN",
                            sample_colname="WEIGHT", species_colname="SPECIES_CODE",
                            species_subset=species.codes,
                            if_multiple_records="First",
                            Method="Fast")

# Combine
# names(dat.akfin) <- c("StationID","EventCode","SampleYear", "GearCode",
#                       "TrawlPerformance_Code.", "EQ.Latitude","EQ.Longitude","HaversineDistance2",
#                       "AvgNetHorizontalOpening","Effort_area_km2", "TSN","CommonName",
#                       "LHSCode","GearDescription","TrawlPerformance","LIFEHISTORY_STAGE",
#                       "TotalCatchNum", "TotalCatchWt")

names(dat.input)
species.dat <- dat.input %>% dplyr::select(StationID, SampleYear, HaulDate, 
                                             EQTime, `EQ Latitude`, `EQ Longitude`,
                                             CommonName, LHSCode,
                                             TotalCatchNum, TotalCatchWt, Effort) %>% 
                             rename(EQ_Latitude=`EQ Latitude`, EQ_Longitude=`EQ Longitude`,
                                    Effort_area_KM2=Effort)

str(species.dat)

# Filter out observations with missing values

if(type=="biom") {
  temp.dat <- species.dat %>% filter(!is.na(EQ_Longitude),
                                     !is.na(EQ_Latitude),
                                     !is.na(Effort_area_KM2),
                                     Effort_area_KM2>0,
                                     !is.na(TotalCatchWt)) #,
                                     # TRAWLPERFORMANCE %in% c("Good/Satisfactory", "Good", "Satisfactory", "G"))
}else {
  temp.dat <- species.dat %>% filter(!is.na(EQ_Longitude),
                                     !is.na(EQ_Latitude),
                                     !is.na(Effort_area_KM2),
                                     Effort_area_KM2>0,
                                     !is.na(TotalCatchNum))#,
                                     # TRAWLPERFORMANCE %in% c("Good/Satisfactory", "Good", "Satisfactory", "G"))
}

# Write.csv
write.csv(temp.dat, file=file.path(dir.output, "InputData.csv"))

# Update with cpue
if(type=="biom") {
  temp.dat <- temp.dat %>% mutate("CPUE"=TotalCatchWt/Effort_area_KM2)
}else {
  temp.dat <- temp.dat %>% mutate("CPUE"=TotalCatchNum/Effort_area_KM2)
}


# Plot Data ====================================================================
if(plot.data==TRUE) {
  # Plot catch histogram
  g.hist <- temp.dat %>% ggplot(aes(x=log(CPUE), fill=log(CPUE))) +
    theme_linedraw() +
    scale_fill_viridis_c() +
    geom_freqpoly() +
    facet_wrap(~SampleYear) +
    theme(legend.position = "top")
  
  g.hist
  
  # g.ridge <- temp.dat %>% ggplot(aes(x=log(CPUE), y=factor(SAMPLEYEAR),
  # fill=log(CPUE))) +
  # theme_linedraw() +
  # scale_fill_viridis_c()
  # geom_density_ridges_gradient()
  # facet_wrap(~SAMPLEYEAR) +
  # theme(legend.position = "top")
  
  # g.ridge
  
  g <- temp.dat %>% ggplot(aes(x=EQ_Longitude, y=EQ_Latitude, color=log(CPUE))) +
    theme_linedraw() +
    scale_color_viridis_c() +
    geom_point(alpha=0.25) +
    facet_wrap(~SampleYear) +
    theme(legend.position = "top")
  
  g
  
  # Plot Map: Density ============================================================
  world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  
  g <- ggplot(data = world) +
    geom_density_2d_filled(data=temp.dat, aes(x=EQ_Longitude, y=EQ_Latitude), show.legend = FALSE, alpha=0.5)+
    
    geom_sf() +
    coord_sf(xlim = c(min(temp.dat$EQ_Longitude), max(temp.dat$EQ_Longitude)), 
             ylim = c(min(temp.dat$EQ_Latitude), max(temp.dat$EQ_Latitude)), expand = TRUE) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "aliceblue"), 
          legend.background = element_blank()) +
    # geom_point(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE)), alpha=0.5) +
    scale_color_viridis_c()
  
  ggsave(filename=file.path(dir.vast, "Data Density Map.png"), plot=g, height=7, width=5, units="in")
  
  # Separate panels by year
  g <- ggplot(data = world) +
    geom_density_2d_filled(data=temp.dat, aes(x=EQ_Longitude, y=EQ_Latitude), show.legend = FALSE, alpha=0.5)+
    
    geom_sf() +
    coord_sf(xlim = c(min(temp.dat$EQ_Longitude), max(temp.dat$EQ_Longitude)), 
             ylim = c(min(temp.dat$EQ_Latitude), max(temp.dat$EQ_Latitude)), expand = TRUE) +
    # ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    # ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
    # pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "aliceblue"), 
          legend.background = element_blank()) +
    # geom_point(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE)), alpha=0.5) +
    scale_color_viridis_c() +
    facet_wrap(~SampleYear)
  
  ggsave(filename=file.path(dir.vast, "Data Density Map_all.png"), plot=g, height=10, width=8, units="in")
  
  
  
  
  # Plot Map: Points ============================================================
  world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
  
  g <- ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(min(temp.dat$EQ_Longitude), max(temp.dat$EQ_Longitude)+5), 
             ylim = c(min(temp.dat$EQ_Latitude), max(temp.dat$EQ_Latitude)), expand = TRUE) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "aliceblue"), 
          legend.background = element_blank()) +
    geom_point(data=temp.dat, aes(x=EQ_Longitude, y=EQ_Latitude, color=log(CPUE)), alpha=0.5) +
    scale_color_viridis_c() +
    xlab("Longitude") + ylab("Latitude")
  
  ggsave(filename=file.path(dir.vast, "Data Points Map_wide.png"), plot=g, height=7, width=6, units="in")
  
  # Separate panels by year
  g <- ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(min(temp.dat$EQ_Longitude), max(temp.dat$EQ_Longitude)+5), 
             ylim = c(min(temp.dat$EQ_Latitude), max(temp.dat$EQ_Latitude)), expand = TRUE) +
    theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "aliceblue"), 
          legend.background = element_blank()) +
    geom_point(data=temp.dat, aes(x=EQ_Longitude, y=EQ_Latitude, color=log(CPUE)), alpha=0.5) +
    scale_color_viridis_c() +
    facet_wrap(~SampleYear) +
    xlab("Longitude") + ylab("Latitude")
  
  ggsave(filename=file.path(dir.vast, "Data Points Map_all.png"), plot=g, height=10, width=8, units="in")
  
  # Presentation:
  g <- ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(min(temp.dat$EQ_Longitude), max(temp.dat$EQ_Longitude)+5), 
             ylim = c(53, max(temp.dat$EQ_Latitude)), expand = TRUE) +
    # theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
    #       panel.background = element_rect(fill = "aliceblue"), 
    #       legend.background = element_blank()) +
    geom_point(data=temp.dat, aes(x=EQ_Longitude, y=EQ_Latitude, color=log(CPUE)), alpha=0.5) +
    scale_color_viridis_c() +
    facet_wrap(~SampleYear, nrow=3) +
    xlab("Longitude") + ylab("Latitude") +
    theme_linedraw() +
    scale_x_continuous(breaks=c(-180, -170, -160, -150))
  g
  
  ggsave(filename=file.path(dir.vast, "Data Points Map_all_presentation.png"), plot=g, height=6, width=10, units="in")
  
  # Lower quality
  ggsave(filename=file.path(dir.vast, "Data Points Map_all_presentation_low.png"), plot=g, height=6, width=10, units="in", dpi=200)
  
}

# Strata Definitions ===========================================================


# Sabrina's Example
# mutate( FinalStrat = case_when( (stratum == 1) & (EQLongitude > -166.75) | (EQLongitude < -172.5) ~ 999,
# TRUE ~ stratum ) )


# Revisions from Sabrina March 29, 2023
# NBS: 60 - 64.15, no long restrictions
# SBS stratum: 58 - 60, -166.75 to -172.5

# strata.limits <- data.frame(
#   'STRATA' = c("All areas","NBS","SBS"),
#   'west_border' = c(-Inf, -Inf, -172.5),
#   'east_border' = c(Inf, Inf, -166.75),
#   'north_border' = c(Inf, 64.15, 60),
#   'south_border' = c(-Inf, 60, 58)
# )

# Six strata
strata.limits <- data.frame(
  'STRATA' = c("All areas","Strata1","Strata2","Strata3","Strata4","Strata5","Strata6"),
  'west_border' = c(-Inf, -172.5, -172.5, -172.5, -166.75, -172.5, -166.75),
  'east_border' = c(Inf, Inf, Inf, -166.75, Inf, -166.75, Inf),
  'north_border' = c(Inf, 66.0, 64.1, 59.9, 59.9, 57.9, 57.9),
  'south_border' = c(-Inf, 64.1, 59.9, 57.9, 57.9, 54.9, 54.9)
)

strata.limits

# Write Strata limits
write.csv(strata.limits, file.path(dir.vast, "strata.limits.csv"))

# strata.limits <- data.frame(
#   'STRATA' = c("All areas","Chum_Garcia"),
#   # 'west_border' = c(Inf, -Inf),
#   # 'east_border' = c(Inf, -Inf),
#   'north_border' = c(Inf, 64.15),
#   'south_border' = c(-Inf, -Inf)
#   )


# strata.limits <- data.frame(
#   'STRATA' = c("All areas","Chum_Garcia"),
#   'west_border' = c(-Inf, Inf),
#   'east_border' = c(-Inf, Inf),
#   'north_border' = c(Inf, 64.15),
#   # 'north_border' = c(Inf, 61.15)
#   'south_border' = c(-Inf, -Inf)
#   )

# Fit VAST Model ===============================================================
setwd(dir.vast)

start.time <- date()

settings <- make_settings( n_x = n_x, 
                           Region=Region,
                           purpose = "index2", 
                           strata.limits = strata.limits, #Updated
                           fine_scale = fine_scale, 
                           bias.correct = TRUE,
                           ObsModel=ObsModel,
                           RhoConfig=RhoConfig,
                           treat_nonencounter_as_zero=treat_nonencounter_as_zero)

user_region <- readRDS(file.path(dir.data, "user_region_ALL.rds"))

# Run model
if(do.est==TRUE) {
  if(type=="biom") {
    fit <- fit_model(settings = settings, 
                     Lat_i = temp.dat$EQ_Latitude, 
                     Lon_i = temp.dat$EQ_Longitude, 
                     t_i = temp.dat$SampleYear, 
                     c_i = rep(0,nrow(temp.dat)),
                     b_i = as_units(temp.dat$TotalCatchWt/1e3, 'kg'), 
                     a_i = as_units(temp.dat$Effort_area_KM2, 'km^2'),
                     input_grid = user_region,
                     grid_dim_km = c(10,10))
  }else {
    fit <- fit_model(settings = settings, 
                     Lat_i = temp.dat$EQ_Latitude, 
                     Lon_i = temp.dat$EQ_Longitude, 
                     t_i = temp.dat$SampleYear, 
                     c_i = rep(0,nrow(temp.dat)),
                     b_i = as_units(temp.dat$TotalCatchNum, 'count'), 
                     a_i = as_units(temp.dat$Effort_area_KM2, 'km^2'),
                     input_grid = user_region,
                     grid_dim_km = c(10,10))
  }
  # Save fit object
  saveRDS(fit, file=file.path(dir.vast, "fit.rds"))
}else {
  fit <- readRDS(file=file.path(dir.vast, "fit.rds"))
}
# fit <- fit_model( "settings" = settings, 
#                   "Lat_i" = temp.dat[,'EQ.Latitude'], 
#                   "Lon_i" = temp.dat[,'EQ.Longitude'], 
#                   "t_i" = temp.dat[,'SampleYear'], 
#                   "c_i" = rep(0,nrow(temp.dat)),
#                   "b_i" = as_units(temp.dat[,'TotalCatchWt'],'kg'), 
#                   "a_i" = temp.dat[,'Effort_area_km2'],
#                   "observations_LL" = coords)

typeof(temp.dat$TotalCatchWt)

# Plot Output ==================================================================
plot(fit)
# plot_results(fit)
# plot_results(fit=fit, plot_1set=3, working_dir=paste0(dir.vast, "/"))

# Plot Encounter Probability
# plot_results(fit=fit, plot_set = c(1,2,3,4,5,6,7,8,9), working_dir=paste0(dir.vast, "/"))

# Comprehensive plotting =======================================================


# Reset working directory ======================================================
setwd(here())

# Timings ===========
end.time <- date()

print(paste("n_x", n_x, 
            "ObsModel", ObsModel[1], ObsModel[2],
            "fine_scale", fine_scale,
            "RhoConfig", RhoConfig[1], RhoConfig[2], RhoConfig[3], RhoConfig[4]))
print(start.time)
print(end.time)

