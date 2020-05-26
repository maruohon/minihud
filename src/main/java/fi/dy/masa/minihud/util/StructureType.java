package fi.dy.masa.minihud.util;

import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Locale;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.minecraft.util.Identifier;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.registry.RegistryKey;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.gen.feature.StructureFeature;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    BURIED_TREASURE     (StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE,     "buried_treasure",      DimensionType.OVERWORLD_REGISTRY_KEY),
    DESERT_PYRAMID      (StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID,      "desert_pyramid",       DimensionType.OVERWORLD_REGISTRY_KEY),
    IGLOO               (StructureToggle.OVERLAY_STRUCTURE_IGLOO,               "igloo",                DimensionType.OVERWORLD_REGISTRY_KEY),
    JUNGLE_TEMPLE       (StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE,       "jungle_pyramid",       DimensionType.OVERWORLD_REGISTRY_KEY),
    MANSION             (StructureToggle.OVERLAY_STRUCTURE_MANSION,             "mansion",              DimensionType.OVERWORLD_REGISTRY_KEY),
    MINESHAFT           (StructureToggle.OVERLAY_STRUCTURE_MINESHAFT,           "mineshaft",            DimensionType.OVERWORLD_REGISTRY_KEY),
    OCEAN_MONUMENT      (StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT,      "monument",             DimensionType.OVERWORLD_REGISTRY_KEY),
    OCEAN_RUIN          (StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN,          "ocean_ruin",           DimensionType.OVERWORLD_REGISTRY_KEY),
    PILLAGER_OUTPOST    (StructureToggle.OVERLAY_STRUCTURE_PILLAGER_OUTPOST,    "pillager_outpost",     DimensionType.OVERWORLD_REGISTRY_KEY),
    SHIPWRECK           (StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK,           "shipwreck",            DimensionType.OVERWORLD_REGISTRY_KEY),
    STRONGHOLD          (StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD,          "stronghold",           DimensionType.OVERWORLD_REGISTRY_KEY),
    VILLAGE             (StructureToggle.OVERLAY_STRUCTURE_VILLAGE,             "village",              DimensionType.OVERWORLD_REGISTRY_KEY),
    WITCH_HUT           (StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT,           "swamp_hut",            DimensionType.OVERWORLD_REGISTRY_KEY),

    RUINED_PORTAL       (StructureToggle.OVERLAY_STRUCTURE_RUINED_PORTAL,       "ruined_portal",        DimensionType.OVERWORLD_REGISTRY_KEY, DimensionType.THE_NETHER_REGISTRY_KEY),

    BASTION_REMNANT     (StructureToggle.OVERLAY_STRUCTURE_BASTION_REMNANT,     "bastion_remnant",      DimensionType.THE_NETHER_REGISTRY_KEY),
    NETHER_FOSSIL       (StructureToggle.OVERLAY_STRUCTURE_NETHER_FOSSIL,       "nether_fossil",        DimensionType.THE_NETHER_REGISTRY_KEY),
    NETHER_FORTRESS     (StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS,     "fortress",             DimensionType.THE_NETHER_REGISTRY_KEY),

    END_CITY            (StructureToggle.OVERLAY_STRUCTURE_END_CITY,            "endcity",              DimensionType.THE_END_REGISTRY_KEY);

    public static final ImmutableList<StructureType> VALUES;
    private static final HashMap<String, StructureType> ID_TO_TYPE = new HashMap<>();

    static
    {
        VALUES = ImmutableList.copyOf(values());

        for (StructureType type : VALUES)
        {
            StructureFeature<?> feature = StructureFeature.field_24842.get(type.structureName.toLowerCase(Locale.ROOT));

            if (feature != null)
            {
                Identifier key = Registry.STRUCTURE_FEATURE.getId(feature);

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
    private final ImmutableSet<RegistryKey<DimensionType>> dimIds;

    StructureType(StructureToggle toggle, String structureName, RegistryKey<DimensionType>... dimIds)
    {
        this.structureName = structureName;
        this.toggle = toggle;
        this.dimIds = ImmutableSet.copyOf(dimIds);
    }

    public boolean existsInDimension(RegistryKey<DimensionType> dimId)
    {
        return this.dimIds.contains(dimId);
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
}
