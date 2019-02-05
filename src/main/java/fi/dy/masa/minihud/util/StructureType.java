package fi.dy.masa.minihud.util;

import javax.annotation.Nullable;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    DESERT_PYRAMID      ( 0, "Temple",      "TeDP", StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID),
    IGLOO               ( 0, "Temple",      "Iglu", StructureToggle.OVERLAY_STRUCTURE_IGLOO),
    JUNGLE_TEMPLE       ( 0, "Temple",      "TeJP", StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE),
    MANSION             ( 0, "Mansion",     "",     StructureToggle.OVERLAY_STRUCTURE_MANSION),
    OCEAN_MONUMENT      ( 0, "Monument",    "",     StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT),
    STRONGHOLD          ( 0, "Stronghold",  "",     StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD),
    VILLAGE             ( 0, "Village",     "",     StructureToggle.OVERLAY_STRUCTURE_VILLAGE),
    WITCH_HUT           ( 0, "Temple",      "TeSH", StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT),

    NETHER_FORTRESS     (-1, "Fortress",    "",     StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS),

    END_CITY            ( 1, "EndCity",     "",     StructureToggle.OVERLAY_STRUCTURE_END_CITY);

    private final StructureToggle toggle;
    private final String structureName;
    private final String componentId;
    private final int dimId;

    private StructureType(int dimId, String structureName, String componentId, StructureToggle toggle)
    {
        this.structureName = structureName;
        this.componentId = componentId;
        this.toggle = toggle;
        this.dimId = dimId;
    }

    public boolean existsInDimension(int dimension)
    {
        return this.dimId == dimension;
    }

    public String getStructureName()
    {
        return this.structureName;
    }

    public StructureToggle getToggle()
    {
        return this.toggle;
    }

    public boolean isEnabled()
    {
        return this.toggle.getToggleOption().getBooleanValue();
    }

    @Nullable
    public static StructureType templeTypeFromComponentId(String id)
    {
        for (StructureType type : values())
        {
            if (type.componentId.equals(id))
            {
                return type;
            }
        }

        return null;
    }
}
