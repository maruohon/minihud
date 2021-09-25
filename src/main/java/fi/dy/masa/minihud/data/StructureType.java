package fi.dy.masa.minihud.data;

import javax.annotation.Nullable;
import com.google.common.collect.ImmutableMap;
import net.minecraft.world.DimensionType;
import net.minecraft.world.WorldProvider;
import net.minecraft.world.WorldProviderEnd;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    DESERT_PYRAMID      (DimensionType.OVERWORLD,   "Temple",      "TeDP", StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID),
    IGLOO               (DimensionType.OVERWORLD,   "Temple",      "Iglu", StructureToggle.OVERLAY_STRUCTURE_IGLOO),
    JUNGLE_TEMPLE       (DimensionType.OVERWORLD,   "Temple",      "TeJP", StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE),
    MANSION             (DimensionType.OVERWORLD,   "Mansion",     "",     StructureToggle.OVERLAY_STRUCTURE_MANSION),
    OCEAN_MONUMENT      (DimensionType.OVERWORLD,   "Monument",    "",     StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT),
    STRONGHOLD          (DimensionType.OVERWORLD,   "Stronghold",  "",     StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD),
    VILLAGE             (DimensionType.OVERWORLD,   "Village",     "",     StructureToggle.OVERLAY_STRUCTURE_VILLAGE),
    WITCH_HUT           (DimensionType.OVERWORLD,   "Temple",      "TeSH", StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT),

    NETHER_FORTRESS     (DimensionType.NETHER,      "Fortress",    "",     StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS),

    END_CITY            (DimensionType.THE_END,     "EndCity",     "",     StructureToggle.OVERLAY_STRUCTURE_END_CITY);

    public static final StructureType[] VALUES = StructureType.values();

    public static final ImmutableMap<String, StructureType> ID_TO_TYPE = buildIdToTypeMap();

    private final StructureToggle toggle;
    private final String structureName;
    private final String componentId;
    private final DimensionType dimType;
    private final boolean isTemple;

    StructureType(DimensionType dimType, String structureName, String componentId, StructureToggle toggle)
    {
        this.structureName = structureName;
        this.componentId = componentId;
        this.toggle = toggle;
        this.dimType = dimType;
        this.isTemple = componentId.isEmpty() == false;
    }

    public boolean existsInDimension(WorldProvider provider)
    {
        if (provider.isSurfaceWorld())
        {
            return this.dimType == DimensionType.OVERWORLD;
        }
        else if (provider.isNether())
        {
            return this.dimType == DimensionType.NETHER;
        }
        else if (provider instanceof WorldProviderEnd)
        {
            return this.dimType == DimensionType.THE_END;
        }

        return false;
    }

    public String getStructureName()
    {
        return this.structureName;
    }

    public boolean isTemple()
    {
        return this.isTemple;
    }

    public StructureToggle getToggle()
    {
        return this.toggle;
    }

    public boolean isEnabled()
    {
        return this.toggle.isEnabled();
    }

    @Nullable
    public static StructureType templeTypeFromComponentId(String id)
    {
        for (StructureType type : VALUES)
        {
            if (type.componentId.equals(id))
            {
                return type;
            }
        }

        return null;
    }

    private static ImmutableMap<String, StructureType> buildIdToTypeMap()
    {
        ImmutableMap.Builder<String, StructureType> builder = ImmutableMap.builder();

        for (StructureType type : VALUES)
        {
            if (type.componentId.isEmpty() == false)
            {
                builder.put(type.structureName + "." + type.componentId, type);
            }
            else
            {
                builder.put(type.structureName, type);
            }
        }

        return builder.build();
    }
}
