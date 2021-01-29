package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseOptionListConfigValue;

public class BlockGridMode extends BaseOptionListConfigValue
{
    public static final BlockGridMode NON_AIR  = new BlockGridMode("non_air",     "minihud.label.blockgridmode.non_air");
    public static final BlockGridMode ADJACENT = new BlockGridMode("adjacent",    "minihud.label.blockgridmode.adjacent");
    public static final BlockGridMode ALL      = new BlockGridMode("all",         "minihud.label.blockgridmode.all");

    public static final ImmutableList<BlockGridMode> VALUES = ImmutableList.of(NON_AIR, ADJACENT, ALL);

    private BlockGridMode(String name, String translationKey)
    {
        super(name, translationKey);
    }
}
