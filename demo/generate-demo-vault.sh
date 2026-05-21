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

# Create Abiotic notes
create_note "ecosystem/abiotic/Sunlight.md" "Sunlight" "Photosynthesis"
create_note "ecosystem/abiotic/Soil.md" "Soil" "Decomposer" "NutrientCycle"

# Create Trophic notes
create_note "ecosystem/trophic/Producer.md" "Producer" "Sunlight" "Photosynthesis"
create_note "ecosystem/trophic/Decomposer.md" "Decomposer" "Soil" "NutrientCycle"

# Create Organism notes (The links create the graph topology)
create_note "ecosystem/organisms/OakTree.md" "OakTree" "Producer" "Sunlight" "Soil"
create_note "ecosystem/organisms/Fungi.md" "Fungi" "Decomposer" "Soil"
create_note "ecosystem/organisms/Earthworm.md" "Earthworm" "Decomposer" "Soil"
create_note "ecosystem/organisms/Squirrel.md" "Squirrel" "OakTree" "Producer"

# Create core meta notes
create_note "ecosystem/Photosynthesis.md" "Photosynthesis" "Sunlight" "Producer"
create_note "ecosystem/NutrientCycle.md" "NutrientCycle" "Decomposer" "Soil"

echo "Demo ecosystem vault generated in ./ecosystem"
