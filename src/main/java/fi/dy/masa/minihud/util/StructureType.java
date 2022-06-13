package fi.dy.masa.minihud.util;

import java.util.HashMap;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.minecraft.util.Identifier;
import net.minecraft.world.World;
import net.minecraft.world.dimension.DimensionType;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    BURIED_TREASURE     (StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE,     "buried_treasure",      World.OVERWORLD.getRegistry()),
    DESERT_PYRAMID      (StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID,      "desert_pyramid",       World.OVERWORLD.getRegistry()),
    IGLOO               (StructureToggle.OVERLAY_STRUCTURE_IGLOO,               "igloo",                World.OVERWORLD.getRegistry()),
    JUNGLE_TEMPLE       (StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE,       "jungle_pyramid",       World.OVERWORLD.getRegistry()),
    MANSION             (StructureToggle.OVERLAY_STRUCTURE_MANSION,             "mansion",              World.OVERWORLD.getRegistry()),
    MINESHAFT           (StructureToggle.OVERLAY_STRUCTURE_MINESHAFT,           "mineshaft",            World.OVERWORLD.getRegistry()),
    OCEAN_MONUMENT      (StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT,      "monument",             World.OVERWORLD.getRegistry()),
    OCEAN_RUIN          (StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN,          "ocean_ruin",           World.OVERWORLD.getRegistry()),
    PILLAGER_OUTPOST    (StructureToggle.OVERLAY_STRUCTURE_PILLAGER_OUTPOST,    "pillager_outpost",     World.OVERWORLD.getRegistry()),
    SHIPWRECK           (StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK,           "shipwreck",            World.OVERWORLD.getRegistry()),
    STRONGHOLD          (StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD,          "stronghold",           World.OVERWORLD.getRegistry()),
    VILLAGE             (StructureToggle.OVERLAY_STRUCTURE_VILLAGE,             "village",              World.OVERWORLD.getRegistry()),
    WITCH_HUT           (StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT,           "swamp_hut",            World.OVERWORLD.getRegistry()),

    RUINED_PORTAL       (StructureToggle.OVERLAY_STRUCTURE_RUINED_PORTAL,       "ruined_portal",        World.OVERWORLD.getRegistry(), World.NETHER.getRegistry()),

    BASTION_REMNANT     (StructureToggle.OVERLAY_STRUCTURE_BASTION_REMNANT,     "bastion_remnant",      World.NETHER.getRegistry()),
    NETHER_FOSSIL       (StructureToggle.OVERLAY_STRUCTURE_NETHER_FOSSIL,       "nether_fossil",        World.NETHER.getRegistry()),
    NETHER_FORTRESS     (StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS,     "fortress",             World.NETHER.getRegistry()),

    END_CITY            (StructureToggle.OVERLAY_STRUCTURE_END_CITY,            "endcity",              World.END.getRegistry());

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
