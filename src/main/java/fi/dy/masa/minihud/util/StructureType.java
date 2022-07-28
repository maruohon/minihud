package fi.dy.masa.minihud.util;

import java.util.HashMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    BURIED_TREASURE     (StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE,     "minecraft:buried_treasure"),
    DESERT_PYRAMID      (StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID,      "minecraft:desert_pyramid"),
    IGLOO               (StructureToggle.OVERLAY_STRUCTURE_IGLOO,               "minecraft:igloo"),
    JUNGLE_TEMPLE       (StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE,       "minecraft:jungle_pyramid"),
    MANSION             (StructureToggle.OVERLAY_STRUCTURE_MANSION,             "minecraft:mansion"),
    MINESHAFT           (StructureToggle.OVERLAY_STRUCTURE_MINESHAFT,           "minecraft:mineshaft", "minecraft:mineshaft_mesa"),
    OCEAN_MONUMENT      (StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT,      "minecraft:monument"),
    OCEAN_RUIN          (StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN,          "minecraft:ocean_ruin_cold", "minecraft:ocean_ruin_warm"),
    PILLAGER_OUTPOST    (StructureToggle.OVERLAY_STRUCTURE_PILLAGER_OUTPOST,    "minecraft:pillager_outpost"),
    RUINED_PORTAL       (StructureToggle.OVERLAY_STRUCTURE_RUINED_PORTAL,       "minecraft:ruined_portal", "minecraft:ruined_portal_desert", "minecraft:ruined_portal_jungle", "minecraft:ruined_portal_mountain", "minecraft:ruined_portal_nether", "minecraft:ruined_portal_ocean", "minecraft:ruined_portal_swamp"),
    SHIPWRECK           (StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK,           "minecraft:shipwreck", "minecraft:shipwreck_beached"),
    STRONGHOLD          (StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD,          "minecraft:stronghold"),
    VILLAGE             (StructureToggle.OVERLAY_STRUCTURE_VILLAGE,             "minecraft:village_desert", "minecraft:village_plains", "minecraft:village_savanna", "minecraft:village_snowy", "minecraft:village_taiga"),
    WITCH_HUT           (StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT,           "minecraft:swamp_hut"),

    BASTION_REMNANT     (StructureToggle.OVERLAY_STRUCTURE_BASTION_REMNANT,     "minecraft:bastion_remnant"),
    NETHER_FORTRESS     (StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS,     "minecraft:fortress"),
    NETHER_FOSSIL       (StructureToggle.OVERLAY_STRUCTURE_NETHER_FOSSIL,       "minecraft:nether_fossil"),

    END_CITY            (StructureToggle.OVERLAY_STRUCTURE_END_CITY,            "minecraft:end_city"),

    UNKNOWN             (StructureToggle.OVERLAY_STRUCTURE_UNKNOWN);

    private static final HashMap<String, StructureType> STRUCTURE_ID_TO_TYPE = new HashMap<>();
    public static final ImmutableList<StructureType> VALUES = ImmutableList.copyOf(values());

    public static StructureType fromStructureId(String id)
    {
        if (STRUCTURE_ID_TO_TYPE.isEmpty())
        {
            VALUES.forEach(st -> st.structureIds.forEach(i -> STRUCTURE_ID_TO_TYPE.put(i, st)));
        }

        return STRUCTURE_ID_TO_TYPE.getOrDefault(id, UNKNOWN);
    }

    private final StructureToggle toggle;
    private final ImmutableSet<String> structureIds;

    StructureType(StructureToggle toggle, String... structuresIds)
    {
        this.toggle = toggle;
        this.structureIds = ImmutableSet.copyOf(structuresIds);
    }

    public StructureToggle getToggle()
    {
        return this.toggle;
    }

    public boolean isEnabled()
    {
        return this.toggle.getToggleOption().getBooleanValue();
    }
}
