package fi.dy.masa.minihud.util;

import java.util.Locale;
import fi.dy.masa.minihud.config.StructureToggle;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.gen.feature.Feature;
import net.minecraft.world.gen.feature.structure.Structure;

public enum StructureType
{
    BURIED_TREASURE     (DimensionType.OVERWORLD,   "Buried_Treasure",  StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE),
    DESERT_PYRAMID      (DimensionType.OVERWORLD,   "Desert_Pyramid",   StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID),
    IGLOO               (DimensionType.OVERWORLD,   "Igloo",            StructureToggle.OVERLAY_STRUCTURE_IGLOO),
    JUNGLE_TEMPLE       (DimensionType.OVERWORLD,   "Jungle_Pyramid",   StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE),
    MANSION             (DimensionType.OVERWORLD,   "Mansion",          StructureToggle.OVERLAY_STRUCTURE_MANSION),
    MINESHAFT           (DimensionType.OVERWORLD,   "Mineshaft",        StructureToggle.OVERLAY_STRUCTURE_MINESHAFT),
    OCEAN_MONUMENT      (DimensionType.OVERWORLD,   "Monument",         StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT),
    OCEAN_RUIN          (DimensionType.OVERWORLD,   "Ocean_Ruin",       StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN),
    SHIPWRECK           (DimensionType.OVERWORLD,   "Shipwreck",        StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK),
    STRONGHOLD          (DimensionType.OVERWORLD,   "Stronghold",       StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD),
    VILLAGE             (DimensionType.OVERWORLD,   "Village",          StructureToggle.OVERLAY_STRUCTURE_VILLAGE),
    WITCH_HUT           (DimensionType.OVERWORLD,   "Swamp_Hut",        StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT),

    NETHER_FORTRESS     (DimensionType.NETHER,      "Fortress",         StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS),

    END_CITY            (DimensionType.THE_END,     "EndCity",          StructureToggle.OVERLAY_STRUCTURE_END_CITY);

    private final StructureToggle toggle;
    private final String structureName;
    private final DimensionType dimType;
    private final Structure<?> structure;

    private StructureType(DimensionType dimType, String structureName, StructureToggle toggle)
    {
        this.structureName = structureName;
        this.toggle = toggle;
        this.dimType = dimType;
        this.structure = Feature.STRUCTURES.get(structureName.toLowerCase(Locale.ROOT));
    }

    public boolean existsInDimension(DimensionType dimensionType)
    {
        return this.dimType == dimensionType;
    }

    public String getStructureName()
    {
        return this.structureName;
    }

    public StructureToggle getToggle()
    {
        return this.toggle;
    }

    public Structure<?> getStructure()
    {
        return this.structure;
    }

    public boolean isEnabled()
    {
        return this.toggle.getToggleOption().getBooleanValue();
    }
}
