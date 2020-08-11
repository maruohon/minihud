package fi.dy.masa.minihud.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import net.minecraft.client.MinecraftClient;
import net.minecraft.util.Identifier;
import net.minecraft.util.registry.Registry;
import net.minecraft.util.registry.RegistryKey;
import net.minecraft.world.dimension.DimensionType;
import net.minecraft.world.gen.feature.StructureFeature;
import fi.dy.masa.minihud.config.StructureToggle;

public enum StructureType
{
    BURIED_TREASURE     (StructureToggle.OVERLAY_STRUCTURE_BURIED_TREASURE,     "buried_treasure",      Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    DESERT_PYRAMID      (StructureToggle.OVERLAY_STRUCTURE_DESERT_PYRAMID,      "desert_pyramid",       Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    IGLOO               (StructureToggle.OVERLAY_STRUCTURE_IGLOO,               "igloo",                Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    JUNGLE_TEMPLE       (StructureToggle.OVERLAY_STRUCTURE_JUNGLE_TEMPLE,       "jungle_pyramid",       Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    MANSION             (StructureToggle.OVERLAY_STRUCTURE_MANSION,             "mansion",              Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    MINESHAFT           (StructureToggle.OVERLAY_STRUCTURE_MINESHAFT,           "mineshaft",            Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    OCEAN_MONUMENT      (StructureToggle.OVERLAY_STRUCTURE_OCEAN_MONUMENT,      "monument",             Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    OCEAN_RUIN          (StructureToggle.OVERLAY_STRUCTURE_OCEAN_RUIN,          "ocean_ruin",           Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    PILLAGER_OUTPOST    (StructureToggle.OVERLAY_STRUCTURE_PILLAGER_OUTPOST,    "pillager_outpost",     Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    SHIPWRECK           (StructureToggle.OVERLAY_STRUCTURE_SHIPWRECK,           "shipwreck",            Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    STRONGHOLD          (StructureToggle.OVERLAY_STRUCTURE_STRONGHOLD,          "stronghold",           Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    VILLAGE             (StructureToggle.OVERLAY_STRUCTURE_VILLAGE,             "village",              Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),
    WITCH_HUT           (StructureToggle.OVERLAY_STRUCTURE_WITCH_HUT,           "swamp_hut",            Collections.singletonList(DimensionType.OVERWORLD_REGISTRY_KEY)),

    RUINED_PORTAL       (StructureToggle.OVERLAY_STRUCTURE_RUINED_PORTAL,       "ruined_portal",        ImmutableList.of(DimensionType.OVERWORLD_REGISTRY_KEY, DimensionType.THE_NETHER_REGISTRY_KEY)),

    BASTION_REMNANT     (StructureToggle.OVERLAY_STRUCTURE_BASTION_REMNANT,     "bastion_remnant",      Collections.singletonList(DimensionType.THE_NETHER_REGISTRY_KEY)),
    NETHER_FOSSIL       (StructureToggle.OVERLAY_STRUCTURE_NETHER_FOSSIL,       "nether_fossil",        Collections.singletonList(DimensionType.THE_NETHER_REGISTRY_KEY)),
    NETHER_FORTRESS     (StructureToggle.OVERLAY_STRUCTURE_NETHER_FORTRESS,     "fortress",             Collections.singletonList(DimensionType.THE_NETHER_REGISTRY_KEY)),

    END_CITY            (StructureToggle.OVERLAY_STRUCTURE_END_CITY,            "endcity",              Collections.singletonList(DimensionType.THE_END_REGISTRY_KEY));

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
    private final ImmutableSet<RegistryKey<DimensionType>> dimKeys;
    private final HashMap<DimensionType, Boolean> dimCache = new HashMap<>();

    StructureType(StructureToggle toggle, String structureName, List<RegistryKey<DimensionType>> dimKeys)
    {
        this.toggle = toggle;
        this.structureName = structureName;
        this.feature = StructureFeature.STRUCTURES.get(structureName.toLowerCase(Locale.ROOT));
        this.dimKeys = ImmutableSet.copyOf(dimKeys);
    }

    public boolean existsInDimension(DimensionType dimId)
    {
        return this.dimCache.computeIfAbsent(dimId, (d) -> {
            Optional<RegistryKey<DimensionType>> optional = MinecraftClient.getInstance().world.getRegistryManager().getDimensionTypes().getKey(d);
            return optional.isPresent() && this.dimKeys.contains(optional.get());
        });
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
