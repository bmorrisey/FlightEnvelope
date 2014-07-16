source('VN.R')

#Note: some parameters may be either single values or vectors, but changing that attribute has not yet been fully tested

#Data Setup
aircraft=list()

#altitude for V-n [ft] [value | vector]
aircraft$h=0

#total weight for V-n [lb] [value | vector]
aircraft$W=10000

#pos and neg CL_max [-] [vector]
aircraft$CLmax=c(-0.5,1.1)

#reference area [ft^2] [value]
aircraft$Sref=1000

#pos and neg load factor [g] [vector]
aircraft$DesignLoadFactor=c(-1.5,4)

#limit airspeed [kts @ SL] [value]
aircraft$Vne=225

#lift curve slope dCL/da [per rad]
aircraft$Cla=0.08 * 180 / pi

#mean geometric chord [ft]
aircraft$mean_geom_chord=15

VN(aircraft)

