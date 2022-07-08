package fi.dy.masa.minihud.util;

import java.util.HashMap;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.minecraft.util.Identifier;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.dimension.DimensionTypes;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    BURIED_TREASURE     (StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE,     "buried_treasure",      DimensionTypes.OVERWORLD_ID),
    DESERT_PYRAMID      (StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID,      "desert_pyramid",       DimensionTypes.OVERWORLD_ID),
    IGLOO               (StructureToggle.OVERLAY_STRUCTURE_IGLOO,               "igloo",                DimensionTypes.OVERWORLD_ID),
    JUNGLE_TEMPLE       (StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE,       "jungle_pyramid",       DimensionTypes.OVERWORLD_ID),
    MANSION             (StructureToggle.OVERLAY_STRUCTURE_MANSION,             "mansion",              DimensionTypes.OVERWORLD_ID),
    MINESHAFT           (StructureToggle.OVERLAY_STRUCTURE_MINESHAFT,           "mineshaft",            DimensionTypes.OVERWORLD_ID),
    OCEAN_MONUMENT      (StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT,      "monument",             DimensionTypes.OVERWORLD_ID),
    OCEAN_RUIN          (StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN,          "ocean_ruin",           DimensionTypes.OVERWORLD_ID),
    PILLAGER_OUTPOST    (StructureToggle.OVERLAY_STRUCTURE_PILLAGER_OUTPOST,    "pillager_outpost",     DimensionTypes.OVERWORLD_ID),
    SHIPWRECK           (StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK,           "shipwreck",            DimensionTypes.OVERWORLD_ID),
    STRONGHOLD          (StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD,          "stronghold",           DimensionTypes.OVERWORLD_ID),
    VILLAGE             (StructureToggle.OVERLAY_STRUCTURE_VILLAGE,             "village",              DimensionTypes.OVERWORLD_ID),
    WITCH_HUT           (StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT,           "swamp_hut",            DimensionTypes.OVERWORLD_ID),

    RUINED_PORTAL       (StructureToggle.OVERLAY_STRUCTURE_RUINED_PORTAL,       "ruined_portal",        DimensionTypes.OVERWORLD_ID, DimensionTypes.THE_NETHER_ID),

    BASTION_REMNANT     (StructureToggle.OVERLAY_STRUCTURE_BASTION_REMNANT,     "bastion_remnant",      DimensionTypes.THE_NETHER_ID),
    NETHER_FOSSIL       (StructureToggle.OVERLAY_STRUCTURE_NETHER_FOSSIL,       "nether_fossil",        DimensionTypes.THE_NETHER_ID),
    NETHER_FORTRESS     (StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS,     "fortress",             DimensionTypes.THE_NETHER_ID),

    END_CITY            (StructureToggle.OVERLAY_STRUCTURE_END_CITY,            "endcity",              DimensionTypes.THE_END_ID);

    public static final ImmutableList<StructureType> VALUES = ImmutableList.copyOf(values());
    private static final HashMap<String, StructureType> ID_TO_TYPE = new HashMap<>();

    static
    {
        for (StructureType type : VALUES)
        {
            ID_TO_TYPE.put(type.featureId.toString(), type);
        }
    }

    @Nullable
    public static StructureType byStructureId(String id)
    {
        return ID_TO_TYPE.get(id);
    }

    private final StructureToggle toggle;
    private final String structureName;
    private final Identifier featureId;
    private final ImmutableSet<Identifier> dims;

    StructureType(StructureToggle toggle, String structureName, Identifier... dims)
    {
        this.toggle = toggle;
        this.structureName = structureName;
        this.featureId = new Identifier(structureName);
        this.dims = ImmutableSet.copyOf(dims);
    }

    public boolean existsInDimension(DimensionType dimId)
    {
        return this.dims.contains(dimId.effects()); // a bit of a meh... but works in vanilla
    }

    public String getStructureName()
    {
        return this.structureName;
    }

    public Identifier getFeatureId()
    {
        return this.featureId;
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
