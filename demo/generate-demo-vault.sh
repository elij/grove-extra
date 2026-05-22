#!/usr/bin/env bash

# Setup directory structure
mkdir -p ecosystem/organisms ecosystem/abiotic ecosystem/trophic

# Function to create a file with wikilinks
create_note() {
    local file=$1
    local title=$2
    shift 2
    cat <<EOF > "$file"
# $title

Tags: #nature #biology

## Description
This note covers the role of $title within the ecosystem.

## Connections
EOF
    for link in "$@"; do
        echo "- [[$link]]" >> "$file"
    done
}

# Create Abiotic notes (10)
create_note "ecosystem/abiotic/Sunlight.md" "Sunlight" "Photosynthesis" "Temperature" "EnergyFlow"
create_note "ecosystem/abiotic/Soil.md" "Soil" "NutrientCycle" "Rocks" "Water" "Decomposer"
create_note "ecosystem/abiotic/Water.md" "Water" "WaterCycle" "Humidity" "Soil"
create_note "ecosystem/abiotic/Air.md" "Air" "CarbonCycle" "Wind"
create_note "ecosystem/abiotic/Rocks.md" "Rocks" "Soil" "Minerals"
create_note "ecosystem/abiotic/Temperature.md" "Temperature" "Climate"
create_note "ecosystem/abiotic/Humidity.md" "Humidity" "WaterCycle" "Climate"
create_note "ecosystem/abiotic/Wind.md" "Wind" "Air" "Climate"
create_note "ecosystem/abiotic/Fire.md" "Fire" "CarbonCycle" "NutrientCycle"
create_note "ecosystem/abiotic/Minerals.md" "Minerals" "Rocks" "Soil"

# Create Trophic notes (10)
create_note "ecosystem/trophic/Producer.md" "Producer" "Photosynthesis" "Sunlight" "PrimaryConsumer"
create_note "ecosystem/trophic/PrimaryConsumer.md" "PrimaryConsumer" "Producer" "SecondaryConsumer" "EnergyFlow"
create_note "ecosystem/trophic/SecondaryConsumer.md" "SecondaryConsumer" "PrimaryConsumer" "TertiaryConsumer" "EnergyFlow"
create_note "ecosystem/trophic/TertiaryConsumer.md" "TertiaryConsumer" "SecondaryConsumer" "ApexPredator" "EnergyFlow"
create_note "ecosystem/trophic/ApexPredator.md" "ApexPredator" "TertiaryConsumer" "Scavenger" "EnergyFlow"
create_note "ecosystem/trophic/Decomposer.md" "Decomposer" "NutrientCycle" "Soil" "Detritivore"
create_note "ecosystem/trophic/Detritivore.md" "Detritivore" "Soil" "Decomposer"
create_note "ecosystem/trophic/Scavenger.md" "Scavenger" "NutrientCycle" "Decomposer"
create_note "ecosystem/trophic/Parasite.md" "Parasite" "EnergyFlow"
create_note "ecosystem/trophic/Mutualist.md" "Mutualist" "Producer"

# Create core meta notes (8)
create_note "ecosystem/Photosynthesis.md" "Photosynthesis" "Sunlight" "Water" "CarbonCycle" "Producer"
create_note "ecosystem/NutrientCycle.md" "NutrientCycle" "Decomposer" "Soil" "Minerals"
create_note "ecosystem/WaterCycle.md" "WaterCycle" "Water" "Air"
create_note "ecosystem/CarbonCycle.md" "CarbonCycle" "Air" "Fire" "Photosynthesis"
create_note "ecosystem/EnergyFlow.md" "EnergyFlow" "Sunlight" "Producer" "PrimaryConsumer"
create_note "ecosystem/Climate.md" "Climate" "Temperature" "Humidity" "Wind"
create_note "ecosystem/Symbiosis.md" "Symbiosis" "Mutualist" "Parasite"
create_note "ecosystem/Evolution.md" "Evolution" "Symbiosis" "Climate"

# Create Organism notes (25)
create_note "ecosystem/organisms/OakTree.md" "OakTree" "Producer" "Sunlight" "Soil" "Squirrel"
create_note "ecosystem/organisms/Grass.md" "Grass" "Producer" "Soil" "Rabbit"
create_note "ecosystem/organisms/Fern.md" "Fern" "Producer" "Water" "Humidity"
create_note "ecosystem/organisms/Moss.md" "Moss" "Producer" "Rocks" "Water"
create_note "ecosystem/organisms/PineTree.md" "PineTree" "Producer" "Soil" "Sunlight"
create_note "ecosystem/organisms/Algae.md" "Algae" "Producer" "Water" "Sunlight"
create_note "ecosystem/organisms/Cyanobacteria.md" "Cyanobacteria" "Producer" "NitrogenFixation"
create_note "ecosystem/organisms/Fungi.md" "Fungi" "Decomposer" "Soil" "Symbiosis"
create_note "ecosystem/organisms/Earthworm.md" "Earthworm" "Detritivore" "Soil"
create_note "ecosystem/organisms/Beetle.md" "Beetle" "Detritivore" "Soil"
create_note "ecosystem/organisms/Ant.md" "Ant" "Scavenger" "Soil"
create_note "ecosystem/organisms/Bee.md" "Bee" "Mutualist" "Producer"
create_note "ecosystem/organisms/Butterfly.md" "Butterfly" "Mutualist" "Producer"
create_note "ecosystem/organisms/Snail.md" "Snail" "PrimaryConsumer" "Water"
create_note "ecosystem/organisms/Squirrel.md" "Squirrel" "PrimaryConsumer" "OakTree" "Fox"
create_note "ecosystem/organisms/Rabbit.md" "Rabbit" "PrimaryConsumer" "Grass" "Fox"
create_note "ecosystem/organisms/Deer.md" "Deer" "PrimaryConsumer" "Grass" "Wolf"
create_note "ecosystem/organisms/Mouse.md" "Mouse" "PrimaryConsumer" "Grass" "Owl"
create_note "ecosystem/organisms/Frog.md" "Frog" "SecondaryConsumer" "Water" "Snake"
create_note "ecosystem/organisms/Snake.md" "Snake" "TertiaryConsumer" "Frog" "Hawk"
create_note "ecosystem/organisms/Hawk.md" "Hawk" "ApexPredator" "Mouse" "Snake"
create_note "ecosystem/organisms/Owl.md" "Owl" "ApexPredator" "Mouse" "Night"
create_note "ecosystem/organisms/Fox.md" "Fox" "SecondaryConsumer" "Rabbit" "Squirrel"
create_note "ecosystem/organisms/Wolf.md" "Wolf" "ApexPredator" "Deer" "Fox"
create_note "ecosystem/organisms/Bear.md" "Bear" "ApexPredator" "Deer" "Fish"

echo "Demo ecosystem vault generated in ./ecosystem"
