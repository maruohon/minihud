package fi.dy.masa.minihud.data.structure;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    DESERT_PYRAMID      (StructureToggle.DESERT_PYRAMID,    "Temple",       "TeDP"),
    IGLOO               (StructureToggle.IGLOO,             "Temple",       "Iglu"),
    JUNGLE_TEMPLE       (StructureToggle.JUNGLE_TEMPLE,     "Temple",       "TeJP"),
    MANSION             (StructureToggle.MANSION,           "Mansion",      ""),
    OCEAN_MONUMENT      (StructureToggle.OCEAN_MONUMENT,    "Monument",     ""),
    STRONGHOLD          (StructureToggle.STRONGHOLD,        "Stronghold",   ""),
    VILLAGE             (StructureToggle.VILLAGE,           "Village",      ""),
    WITCH_HUT           (StructureToggle.SWAMP_HUT,         "Temple",       "TeSH"),

    END_CITY            (StructureToggle.END_CITY,          "EndCity",      ""),

    NETHER_FORTRESS     (StructureToggle.NETHER_FORTRESS,   "Fortress",     ""),

    UNKNOWN             (StructureToggle.UNKNOWN,           "",             "");

    public static final ImmutableList<StructureType> VALUES = ImmutableList.copyOf(values());

    private static ImmutableMap<String, StructureType> STRUCTURE_ID_TO_TYPE;

    private final StructureToggle toggle;
    private final ImmutableSet<String> structureIds;
    private final String structureName;
    private final String componentId;
    private final boolean isTemple;

    StructureType(StructureToggle toggle, String structureName, String componentId)
    {
        this.structureName = structureName;
        this.componentId = componentId;
        this.toggle = toggle;
        this.isTemple = componentId.isEmpty() == false;
        this.structureIds = ImmutableSet.of(structureName);
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

    public static StructureType fromStructureId(String id)
    {
        if (STRUCTURE_ID_TO_TYPE == null)
        {
            STRUCTURE_ID_TO_TYPE = buildIdToTypeMap();
            //VALUES.forEach(st -> st.structureIds.forEach(i -> STRUCTURE_ID_TO_TYPE.put(st.componentId.isEmpty() ? i : i + "." + st.componentId, st)));
        }

        return STRUCTURE_ID_TO_TYPE.getOrDefault(id, UNKNOWN);
    }

    private static ImmutableMap<String, StructureType> buildIdToTypeMap()
    {
        ImmutableMap.Builder<String, StructureType> builder = ImmutableMap.builder();

        for (StructureType type : VALUES)
        {
            for (String id : type.structureIds)
            {
                if (type.componentId.isEmpty() == false)
                {
                    builder.put(id + "." + type.componentId, type);
                }
                else
                {
                    builder.put(id, type);
                }
            }
        }

        return builder.build();
    }
}
