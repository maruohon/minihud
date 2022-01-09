package fi.dy.masa.minihud.util;

import java.util.HashMap;
import java.util.Locale;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.minecraft.util.Identifier;
import net.minecraft.util.registry.Registry;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.gen.feature.StructureFeature;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    BURIED_TREASURE     (StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE,     "buried_treasure",      DimensionType.OVERWORLD_ID),
    DESERT_PYRAMID      (StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID,      "desert_pyramid",       DimensionType.OVERWORLD_ID),
    IGLOO               (StructureToggle.OVERLAY_STRUCTURE_IGLOO,               "igloo",                DimensionType.OVERWORLD_ID),
    JUNGLE_TEMPLE       (StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE,       "jungle_pyramid",       DimensionType.OVERWORLD_ID),
    MANSION             (StructureToggle.OVERLAY_STRUCTURE_MANSION,             "mansion",              DimensionType.OVERWORLD_ID),
    MINESHAFT           (StructureToggle.OVERLAY_STRUCTURE_MINESHAFT,           "mineshaft",            DimensionType.OVERWORLD_ID),
    OCEAN_MONUMENT      (StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT,      "monument",             DimensionType.OVERWORLD_ID),
    OCEAN_RUIN          (StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN,          "ocean_ruin",           DimensionType.OVERWORLD_ID),
    PILLAGER_OUTPOST    (StructureToggle.OVERLAY_STRUCTURE_PILLAGER_OUTPOST,    "pillager_outpost",     DimensionType.OVERWORLD_ID),
    SHIPWRECK           (StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK,           "shipwreck",            DimensionType.OVERWORLD_ID),
    STRONGHOLD          (StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD,          "stronghold",           DimensionType.OVERWORLD_ID),
    VILLAGE             (StructureToggle.OVERLAY_STRUCTURE_VILLAGE,             "village",              DimensionType.OVERWORLD_ID),
    WITCH_HUT           (StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT,           "swamp_hut",            DimensionType.OVERWORLD_ID),

    RUINED_PORTAL       (StructureToggle.OVERLAY_STRUCTURE_RUINED_PORTAL,       "ruined_portal",        DimensionType.OVERWORLD_ID, DimensionType.THE_NETHER_ID),

    BASTION_REMNANT     (StructureToggle.OVERLAY_STRUCTURE_BASTION_REMNANT,     "bastion_remnant",      DimensionType.THE_NETHER_ID),
    NETHER_FOSSIL       (StructureToggle.OVERLAY_STRUCTURE_NETHER_FOSSIL,       "nether_fossil",        DimensionType.THE_NETHER_ID),
    NETHER_FORTRESS     (StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS,     "fortress",             DimensionType.THE_NETHER_ID),

    END_CITY            (StructureToggle.OVERLAY_STRUCTURE_END_CITY,            "endcity",              DimensionType.THE_END_ID);

    public static final ImmutableList<StructureType> VALUES;
    private static final HashMap<String, StructureType> ID_TO_TYPE = new HashMap<>();

    static
    {
        VALUES = ImmutableList.copyOf(values());

        for (StructureType type : VALUES)
        {
            if (type.feature != null)
            {
                Identifier key = Registry.STRUCTURE_FEATURE.getId(type.feature);

                if (key != null)
                {
                    ID_TO_TYPE.put(key.toString(), type);
                }
            }
        }
    }

    @Nullable
    public static StructureType byStructureId(String id)
    {
        return ID_TO_TYPE.get(id);
    }

    private final StructureToggle toggle;
    private final String structureName;
    private final StructureFeature<?> feature;
    private final ImmutableSet<Identifier> dims;

    StructureType(StructureToggle toggle, String structureName, Identifier... dims)
    {
        this.toggle = toggle;
        this.structureName = structureName;
        this.feature = StructureFeature.STRUCTURES.get(structureName.toLowerCase(Locale.ROOT));
        this.dims = ImmutableSet.copyOf(dims);
    }

    public boolean existsInDimension(DimensionType dimId)
    {
        return this.dims.contains(dimId.getEffects()); // a bit of a meh... but works in vanilla
    }

    public String getStructureName()
    {
        return this.structureName;
    }

    public StructureFeature<?> getFeature()
    {
        return this.feature;
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
