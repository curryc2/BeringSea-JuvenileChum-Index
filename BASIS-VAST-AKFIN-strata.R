#==================================================================================================
#Project Name: BASIS SALMON - User Defined Region for BASIS Survey - Juvenile Chum Salmon - with Strata
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 2.21.23
#
#Purpose: To create a general script for fitting a VAST model to BASIS surface trawl data - ACCESSED FROM AKFIN FOR REPRODUCABILITY
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


# detach("package:rnaturalearth", unload=TRUE)

# CONTROL ======================================================================

# Do Estimation?
do.est <- TRUE

# Plot Data
plot.data <- FALSE

# Select Species
# species <- "Sockeye Salmon"
species <- "Chum Salmon"

# Version <- "VAST_v12_0_0"

# Abundance or Biomass
type <- c("biom","abund")[2]

# Number of knots (i.e. spatial complexity)
# n_x <- 250
n_x <- 500
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

ObsModel=c(2,0) #Gamma and Standard Delta Model
RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)


# Best Parameterization ======================
# ObsModel=c(2,1) #Gamma and Poisson-Linked Delta
# RhoConfig=c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0)



# Workflow =====================================================================
dir.data <- here("data")

dir.vast <- file.path(here(), "figs", "AKFIN", "User", species, type,
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
if(n_x==250) {
  file.copy(from=file.path(dir.extrap,"Kmeans_extrapolation-2500.RData"),
            to=file.path(dir.vast, "Kmeans_extrapolation-2500.RData"))
}
if(n_x==500) {
  file.copy(from=file.path(dir.extrap,"Kmeans_extrapolation-5000.RData"),
            to=file.path(dir.vast, "Kmeans_extrapolation-5000.RData"))
}

if(n_x==1000) {
  file.copy(from=file.path(dir.extrap,"Kmeans_extrapolation-10000.RData"),
              to=file.path(dir.vast, "Kmeans_extrapolation-10000.RData"))
}

# Read in Compiled Basis Data ==================================================
dat.akfin <- read.csv(file.path(dir.data,"AKFIN","BASIS FISH Catch ALL0 - Juvenile.csv"), stringsAsFactors=FALSE)
dat.2021 <- read.csv(file.path(dir.data,"AKFIN","Curry_JuvSal_0Catch_2021.csv"), stringsAsFactors=FALSE)
dat.2022 <- read.csv(file.path(dir.data,"AKFIN","Curry_BASIS_Salmon_0Catch_22.csv"), stringsAsFactors=FALSE)
  
names(dat.2022)  
names(dat.2021)
names(dat.akfin)

# Combine
STATIONID <- c(dat.akfin$STATIONID, 
               dat.2021$StationID, dat.2022$StationID)
EVENTCODE <- c(dat.akfin$EVENTCODE, 
               dat.2021$EventCode, dat.2022$EventCode)
SAMPLEYEAR <- c(dat.akfin$SAMPLEYEAR, 
                dat.2021$SampleYear, dat.2022$SampleYear)
GEARCODE <- c(dat.akfin$GEARCODE, 
              dat.2021$GearCode, dat.2022$GearCode)

TRAWLPERFORMANCE_CODE <- c(dat.akfin$TRAWLPERFORMANCE_CODE, 
                           dat.2021$TrawlPerformance_Code., dat.2022$TrawlPreformance)
EQ_LATITUDE <- c(dat.akfin$EQ_LATITUDE, 
                 dat.2021$EQ.Latitude, dat.2022$EQ.Latitude)
EQ_LONGITUDE <- c(dat.akfin$EQ_LONGITUDE, 
                  dat.2021$EQ.Longitude, dat.2022$EQ.Longitude)
HAVERSINEDISTANCE2 <- c(dat.akfin$HAVERSINEDISTANCE2, 
                        dat.2021$HaversineDistance2, dat.2022$HaversineDistance2)

AVGNETHORIZONTALOPENING <- c(dat.akfin$AVGNETHORIZONTALOPENING, 
                             dat.2021$AvgNetHorizontalOpening, dat.2022$AvgNetHorizontalOpening)
EFFORT_AREA_KM2 <- c(dat.akfin$EFFORT_AREA_KM2, 
                     dat.2021$Effort_area_km2, dat.2022$Effort_area_km2)
SPECIESNAME <- c(dat.akfin$SPECIESNAME, 
                 dat.2021$CommonName, dat.2022$CommonName)
LHSCODE <- c(dat.akfin$LHSCODE, 
             dat.2021$LHSCode, dat.2022$LHSCode)

GEAR_DESCRIPTION <- c(dat.akfin$GEAR_DESCRIPTION, 
                      dat.2021$GearDescription, dat.2022$Gear.Description)
TRAWLPERFORMANCE <- c(dat.akfin$TRAWLPERFORMANCE, 
                      dat.2021$TrawlPerformance, dat.2022$TrawlPreformance)
TOTALCATCHNUM <- c(dat.akfin$TOTALCATCHNUM, 
                   dat.2021$TotalCatchNum, dat.2022$TotalCatchNum)
TOTALCATCHWT <- c(dat.akfin$TOTALCATCHWT, 
                  dat.2021$TotalCatchWt, dat.2022$TotalCatchWt)

full_data <- data.frame(STATIONID, EVENTCODE, SAMPLEYEAR, GEARCODE,
                        TRAWLPERFORMANCE_CODE, EQ_LATITUDE, EQ_LONGITUDE, HAVERSINEDISTANCE2,
                        AVGNETHORIZONTALOPENING, EFFORT_AREA_KM2, SPECIESNAME, LHSCODE,
                        GEAR_DESCRIPTION, TRAWLPERFORMANCE, TOTALCATCHNUM, TOTALCATCHWT)

str(full_data)
# Combine
# names(dat.akfin) <- c("StationID","EventCode","SampleYear", "GearCode",
#                       "TrawlPerformance_Code.", "EQ.Latitude","EQ.Longitude","HaversineDistance2",
#                       "AvgNetHorizontalOpening","Effort_area_km2", "TSN","CommonName",
#                       "LHSCode","GearDescription","TrawlPerformance","LIFEHISTORY_STAGE",
#                       "TotalCatchNum", "TotalCatchWt")
# names(dat.akfin)
# names(dat.2021)
# 
# str(dat.akfin)
# str(dat.2021)
# 
# full_data <- dat.2021 %>% bind_rows(dat.akfin)
# str(full_data)

species.dat <- subset(full_data, full_data$SPECIESNAME == species)

str(species.dat)

# Filter out observations with missing values

if(type=="biom") {
  temp.dat <- species.dat %>% filter(!is.na(EQ_LONGITUDE),
                                     !is.na(EQ_LATITUDE),
                                     !is.na(EFFORT_AREA_KM2),
                                     EFFORT_AREA_KM2>0,
                                     !is.na(TOTALCATCHWT),
                                     TRAWLPERFORMANCE %in% c("Good/Satisfactory", "Good", "Satisfactory", "G"))
}else {
  temp.dat <- species.dat %>% filter(!is.na(EQ_LONGITUDE),
                                     !is.na(EQ_LATITUDE),
                                     !is.na(EFFORT_AREA_KM2),
                                     EFFORT_AREA_KM2>0,
                                     !is.na(TOTALCATCHNUM),
                                     TRAWLPERFORMANCE %in% c("Good/Satisfactory", "Good", "Satisfactory", "G"))
}


# Update with cpue
if(type=="biom") {
  temp.dat <- temp.dat %>% mutate("CPUE"=TOTALCATCHWT/EFFORT_AREA_KM2)
}else {
  temp.dat <- temp.dat %>% mutate("CPUE"=TOTALCATCHNUM/EFFORT_AREA_KM2)
}


# Plot Data ====================================================================
if(plot.data==TRUE) {
# Plot catch histogram
g.hist <- temp.dat %>% ggplot(aes(x=log(CPUE), fill=log(CPUE))) +
  theme_linedraw() +
  scale_fill_viridis_c() +
  geom_freqpoly() +
  facet_wrap(~SAMPLEYEAR) +
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

g <- temp.dat %>% ggplot(aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE))) +
  theme_linedraw() +
  scale_color_viridis_c() +
  geom_point(alpha=0.25) +
  facet_wrap(~SAMPLEYEAR) +
  theme(legend.position = "top")

g

# Plot Map: Density ============================================================
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

g <- ggplot(data = world) +
  geom_density_2d_filled(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE), show.legend = FALSE, alpha=0.5)+
  
  geom_sf() +
  coord_sf(xlim = c(min(temp.dat$EQ_LONGITUDE), max(temp.dat$EQ_LONGITUDE)), 
           ylim = c(min(temp.dat$EQ_LATITUDE), max(temp.dat$EQ_LATITUDE)), expand = TRUE) +
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
  geom_density_2d_filled(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE), show.legend = FALSE, alpha=0.5)+
  
  geom_sf() +
  coord_sf(xlim = c(min(temp.dat$EQ_LONGITUDE), max(temp.dat$EQ_LONGITUDE)), 
           ylim = c(min(temp.dat$EQ_LATITUDE), max(temp.dat$EQ_LATITUDE)), expand = TRUE) +
  # ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  # ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                    # pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), 
        legend.background = element_blank()) +
  # geom_point(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE)), alpha=0.5) +
  scale_color_viridis_c() +
  facet_wrap(~SAMPLEYEAR)

ggsave(filename=file.path(dir.vast, "Data Density Map_all.png"), plot=g, height=10, width=8, units="in")




# Plot Map: Points ============================================================
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

g <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min(temp.dat$EQ_LONGITUDE), max(temp.dat$EQ_LONGITUDE)+5), 
           ylim = c(min(temp.dat$EQ_LATITUDE), max(temp.dat$EQ_LATITUDE)), expand = TRUE) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(location = "bl", which_north = "true",
                                    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in")) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), 
        legend.background = element_blank()) +
  geom_point(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE)), alpha=0.5) +
  scale_color_viridis_c() +
  xlab("Longitude") + ylab("Latitude")

ggsave(filename=file.path(dir.vast, "Data Points Map_wide.png"), plot=g, height=7, width=6, units="in")

 # Separate panels by year
g <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min(temp.dat$EQ_LONGITUDE), max(temp.dat$EQ_LONGITUDE)+5), 
           ylim = c(min(temp.dat$EQ_LATITUDE), max(temp.dat$EQ_LATITUDE)), expand = TRUE) +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), 
        legend.background = element_blank()) +
  geom_point(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE)), alpha=0.5) +
  scale_color_viridis_c() +
  facet_wrap(~SAMPLEYEAR) +
  xlab("Longitude") + ylab("Latitude")

ggsave(filename=file.path(dir.vast, "Data Points Map_all.png"), plot=g, height=10, width=8, units="in")

# Presentation:
g <- ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min(temp.dat$EQ_LONGITUDE), max(temp.dat$EQ_LONGITUDE)+5), 
           ylim = c(53, max(temp.dat$EQ_LATITUDE)), expand = TRUE) +
  # theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5), 
  #       panel.background = element_rect(fill = "aliceblue"), 
  #       legend.background = element_blank()) +
  geom_point(data=temp.dat, aes(x=EQ_LONGITUDE, y=EQ_LATITUDE, color=log(CPUE)), alpha=0.5) +
  scale_color_viridis_c() +
  facet_wrap(~SAMPLEYEAR, nrow=3) +
  xlab("Longitude") + ylab("Latitude") +
  theme_linedraw() +
  scale_x_continuous(breaks=c(-180, -170, -160, -150))
g

ggsave(filename=file.path(dir.vast, "Data Points Map_all_presentation.png"), plot=g, height=6, width=10, units="in")

# Lower quality
ggsave(filename=file.path(dir.vast, "Data Points Map_all_presentation_low.png"), plot=g, height=6, width=10, units="in", dpi=200)

}

# Fit VAST Model ===============================================================
setwd(dir.vast)

start.time <- date()

settings <- make_settings( n_x = n_x, 
                           Region=Region,
                           purpose = "index2", 
                           strata.limits = data.frame(STRATA="All_areas"),
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
                     Lat_i = temp.dat$EQ_LATITUDE, 
                     Lon_i = temp.dat$EQ_LONGITUDE, 
                     t_i = temp.dat$SAMPLEYEAR, 
                     c_i = rep(0,nrow(temp.dat)),
                     b_i = as_units(temp.dat$TOTALCATCHWT/1e3, 'kg'), 
                     a_i = as_units(temp.dat$EFFORT_AREA_KM2, 'km^2'),
                     input_grid = user_region,
                     grid_dim_km = c(10,10))
  }else {
   fit <- fit_model(settings = settings, 
                     Lat_i = temp.dat$EQ_LATITUDE, 
                     Lon_i = temp.dat$EQ_LONGITUDE, 
                     t_i = temp.dat$SAMPLEYEAR, 
                     c_i = rep(0,nrow(temp.dat)),
                     b_i = as_units(temp.dat$TOTALCATCHNUM, 'count'), 
                     a_i = as_units(temp.dat$EFFORT_AREA_KM2, 'km^2'),
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

