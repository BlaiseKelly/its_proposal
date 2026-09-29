library(networkD3)
library(dplyr)

# ---------------------------------------------------------------------------
# make_energy_sankey()
#
# Builds a Sankey diagram of energy flow through a vehicle:
#
#   Fuel/electricity in -> [Engine/drivetrain losses]  (heat, friction)
#                        -> Useful mechanical energy -> Drag
#                                                     -> Rolling resistance
#                                                     -> Kinetic energy (accel/decel)
#
# For electric vehicles, kinetic energy is further split so some is
# recovered via regenerative braking (shown as its own downstream box)
# rather than all being lost - this is the key structural difference
# from a petrol car, where kinetic energy at braking is simply lost as
# brake heat.
#
# Unlike the earlier box diagram, a Sankey's link WIDTH already encodes
# magnitude and the total is conserved (everything in = everything out),
# so there's no separate "sizing" step needed - you just supply energy
# values (any consistent unit, e.g. MJ, kWh, or % of total) and the
# widths follow automatically.
#
# Arguments:
#   label          : name shown in the plot title (not rendered by
#                    networkD3 itself, but returned as an attribute you
#                    can print alongside it)
#   total_energy   : total energy input for this scenario (e.g. per 100 km)
#   loss_fraction  : fraction of total_energy lost in engine/drivetrain
#                    before it does any useful work (much higher for
#                    petrol ~0.65-0.75 than electric ~0.10-0.20)
#   drag_frac, rolling_frac, kinetic_frac : how the *useful* mechanical
#                    energy splits between the three resistances (should
#                    sum to 1)
#   regen_frac     : fraction of the kinetic-energy share recovered via
#                    regenerative braking (0 for petrol, >0 for EV/hybrid)
# ---------------------------------------------------------------------------

make_energy_sankey <- function(label = "Scenario",
                               total_energy = 100,
                               loss_fraction = 0.7,
                               drag_frac = 0.35,
                               rolling_frac = 0.35,
                               kinetic_frac = 0.30,
                               regen_frac = 0) {
  
  stopifnot(abs(drag_frac + rolling_frac + kinetic_frac - 1) < 1e-6)
  
  useful   <- total_energy * (1 - loss_fraction)
  lost     <- total_energy * loss_fraction
  
  drag     <- useful * drag_frac
  rolling  <- useful * rolling_frac
  kinetic  <- useful * kinetic_frac
  recovered <- kinetic * regen_frac
  kinetic_lost <- kinetic - recovered
  
  nodes <- data.frame(name = c(
    "Energy in",                 # 0
    "Engine/drivetrain losses",  # 1
    "Useful mechanical energy",  # 2
    "Drag",                      # 3
    "Rolling resistance",        # 4
    "Kinetic energy (accel/brake)", # 5
    "Recovered (regen braking)", # 6
    "Lost as brake heat"         # 7
  ))
  
  links <- data.frame(
    source = c(0, 0, 2, 2, 2),
    target = c(1, 2, 3, 4, 5),
    value  = c(lost, useful, drag, rolling, kinetic)
  )
  
  # only add the regen split if there's anything to recover (keeps a
  # petrol diagram, where regen_frac = 0, tidy with no zero-width links)
  if (regen_frac > 0) {
    links <- rbind(links, data.frame(
      source = c(5, 5),
      target = c(6, 7),
      value  = c(recovered, kinetic_lost)
    ))
  }
  
  sn <- sankeyNetwork(Links = links, Nodes = nodes,
                      Source = "source", Target = "target", Value = "value",
                      NodeID = "name", fontSize = 13, nodeWidth = 22,
                      sinksRight = TRUE)
  attr(sn, "scenario_label") <- label
  sn
}

# ---------------------------------------------------------------------------
# Example: petrol vs electric car over the same trip / same total input
# energy for easy visual comparison. Adjust total_energy per vehicle if
# you want to reflect that an EV typically needs less input energy to
# cover the same distance.
# ---------------------------------------------------------------------------

petrol_sankey <- make_energy_sankey(
  label = "Petrol car",
  total_energy = 100,
  loss_fraction = 0.70,   # ~70% lost as engine heat - typical for ICE
  drag_frac = 0.35, rolling_frac = 0.35, kinetic_frac = 0.30,
  regen_frac = 0          # no regenerative braking
)

electric_sankey <- make_energy_sankey(
  label = "Electric car",
  total_energy = 100,
  loss_fraction = 0.15,   # ~15% lost in motor/battery/inverter - typical for EV
  drag_frac = 0.35, rolling_frac = 0.35, kinetic_frac = 0.30,
  regen_frac = 0.6        # ~60% of braking kinetic energy recovered
)

petrol_sankey
electric_sankey

# To save either as a standalone HTML file:
# library(htmlwidgets)
# saveWidget(petrol_sankey, "petrol_energy_sankey.html", selfcontained = TRUE)
# saveWidget(electric_sankey, "electric_energy_sankey.html", selfcontained = TRUE)

library(networkD3)

# ---------------------------------------------------------------------------
# Simple Sankey: where does the fuel energy actually go?
#
#   Fuel energy in -> Lost as heat
#                  -> Energy to move vehicle + occupant -> Moving the car
#                                                       -> Moving the person
#
# The car/person split is approximated by mass share. That is a fair
# approximation for rolling resistance and acceleration (both scale with
# mass) but not for aerodynamic drag, which does not - so treat the person
# share as an upper-ish estimate rather than an exact figure.
# ---------------------------------------------------------------------------

make_simple_sankey <- function(total_energy = 100,
                               loss_fraction = 0.70,   # ICE: ~0.70, EV: ~0.15
                               vehicle_mass  = 1500,   # kg
                               occupant_mass = 75) {   # kg
  
  lost   <- total_energy * loss_fraction
  useful <- total_energy - lost
  
  person_share <- occupant_mass / (vehicle_mass + occupant_mass)
  person <- useful * person_share
  car    <- useful - person
  
  # put the values in the node names so they show on the chart
  nodes <- data.frame(name = c(
    sprintf("Fuel energy in (%.0f)", total_energy),
    sprintf("Lost as heat (%.1f)", lost),
    sprintf("Energy to move vehicle + occupant (%.1f)", useful),
    sprintf("Moving the car (%.1f)", car),
    sprintf("Moving the person (%.1f)", person)
  ))
  
  links <- data.frame(
    source = c(0, 0, 2, 2),
    target = c(1, 2, 3, 4),
    value  = c(lost, useful, car, person)
  )
  
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target", Value = "value",
                NodeID = "name", fontSize = 13, nodeWidth = 24,
                sinksRight = TRUE)
}

# Petrol car, one occupant
make_simple_sankey(total_energy = 100,
                   loss_fraction = 0.70,   # ICE: ~0.70, EV: ~0.15
                   vehicle_mass  = 1500,   # kg
                   occupant_mass = 75)

# Electric car for comparison (heavier battery, far less lost energy)
make_simple_sankey(loss_fraction = 0.15, vehicle_mass = 1800)

library(networkD3)

# ---------------------------------------------------------------------------
# Speed-dependent energy Sankey
#
#   Fuel energy in -> Lost as heat
#                  -> Useful energy -> Rolling resistance -> car / person
#                                   -> Acceleration (KE)   -> car / person
#                                   -> Air drag            -> car
#
# Energy at the wheels, per metre travelled (J/m = N):
#   rolling      = Crr * m * g              (independent of speed)
#   drag         = 0.5 * rho * Cd * A * v^2  (grows with speed squared)
#   acceleration = 0.5 * m * v^2 * stops_per_km / 1000
#                  (kinetic energy built up at each stop-start cycle, then
#                   dissipated in braking unless recovered by regen)
#
# Rolling and acceleration scale with mass, so the person's share of those
# is occupant_mass / total_mass. Drag does not scale with mass, so it is
# all attributed to the car.
# ---------------------------------------------------------------------------

make_speed_sankey <- function(speed_kmh = 50,
                              stops_per_km = 1,        # 0 for steady motorway cruising
                              loss_fraction = 0.70,    # ICE ~0.70, EV ~0.15
                              vehicle_mass = 1500,     # kg
                              occupant_mass = 75,      # kg
                              Crr = 0.010, Cd = 0.30, frontal_area = 2.2,
                              total_energy = 100) {
  
  g <- 9.81; rho <- 1.2
  v <- speed_kmh / 3.6
  m <- vehicle_mass + occupant_mass
  
  rolling <- Crr * m * g
  drag    <- 0.5 * rho * Cd * frontal_area * v^2
  accel   <- 0.5 * m * v^2 * stops_per_km / 1000
  
  # scale so that the useful energy equals (1 - loss) of the energy in
  useful <- total_energy * (1 - loss_fraction)
  lost   <- total_energy * loss_fraction
  k <- useful / (rolling + drag + accel)
  rolling <- rolling * k; drag <- drag * k; accel <- accel * k
  
  p <- occupant_mass / m                      # person's mass share
  roll_person <- rolling * p; roll_car <- rolling - roll_person
  acc_person  <- accel * p;   acc_car  <- accel - acc_person
  person <- roll_person + acc_person
  car    <- roll_car + acc_car + drag
  
  nodes <- data.frame(name = c(
    sprintf("Fuel energy in (%.0f)", total_energy),   # 0
    sprintf("Lost as heat (%.1f)", lost),             # 1
    sprintf("Useful energy (%.1f)", useful),          # 2
    sprintf("Rolling resistance (%.1f)", rolling),    # 3
    sprintf("Air drag (%.1f)", drag),                 # 4
    sprintf("Acceleration (%.1f)", accel),            # 5
    sprintf("Moving the car (%.1f)", car),            # 6
    sprintf("Moving the person (%.1f)", person)       # 7
  ))
  
  links <- data.frame(
    source = c(0, 0, 2, 2, 2, 3, 3, 4, 5, 5),
    target = c(1, 2, 3, 4, 5, 6, 7, 6, 6, 7),
    value  = c(lost, useful, rolling, drag, accel,
               roll_car, roll_person, drag, acc_car, acc_person)
  )
  links <- links[links$value > 0, ]   # drop zero-width links (e.g. no stops)
  
  sankeyNetwork(Links = links, Nodes = nodes,
                Source = "source", Target = "target", Value = "value",
                NodeID = "name", fontSize = 13, nodeWidth = 24,
                sinksRight = TRUE)
}

# Compare driving conditions for the same petrol car
make_speed_sankey(speed_kmh = 30,  stops_per_km = 2)    # congested urban
make_speed_sankey(speed_kmh = 50,  stops_per_km = 1)    # urban
make_speed_sankey(speed_kmh = 110, stops_per_km = 0)    # motorway cruise

library(networkD3)

# ---------------------------------------------------------------------------
# Kinetic energy Sankey - one acceleration from rest to a set speed.
# No drag, no rolling resistance, no stop-start cycles.
#
#   Fuel energy in -> Lost as heat
#                  -> Car total, incl. passenger -> Car
#                                                -> Person
#
# Kinetic energy = 0.5 * m * v^2, so:
#   - the car/person split is just the mass ratio (speed cancels out)
#   - the SIZE of the whole diagram scales with speed squared
#     (double the speed = four times the energy)
# Fuel energy in = kinetic energy needed / engine efficiency.
# ---------------------------------------------------------------------------

make_ke_sankey <- function(speed_kmh = 50,
                           vehicle_mass = 1500,      # kg
                           occupant_mass = 75,       # kg
                           loss_fraction = 0.70) {   # ICE ~0.70, EV ~0.15
  
  v <- speed_kmh / 3.6                               # m/s
  
  ke_car    <- 0.5 * vehicle_mass  * v^2 / 1000      # kJ
  ke_person <- 0.5 * occupant_mass * v^2 / 1000      # kJ
  ke_total  <- ke_car + ke_person
  
  fuel_in <- ke_total / (1 - loss_fraction)          # kJ
  lost    <- fuel_in - ke_total
  
  nodes <- data.frame(name = c(
    sprintf("Fuel energy in (%.0f kJ)", fuel_in),
    sprintf("Lost as heat (%.0f kJ)", lost),
    sprintf("Car total, incl. passenger (%.0f kJ)", ke_total),
    sprintf("Car (%.0f kJ)", ke_car),
    sprintf("Person (%.1f kJ)", ke_person)
  ))
  
  links <- data.frame(
    source = c(0, 0, 2, 2),
    target = c(1, 2, 3, 4),
    value  = c(lost, ke_total, ke_car, ke_person)
  )
  
  sankeyNetwork(Links = links, Nodes = nodes,fontFamily = "Helvetica",
                Source = "source", Target = "target", Value = "value",
                NodeID = "name", fontSize = 20, nodeWidth = 24,
                sinksRight = TRUE)
}
