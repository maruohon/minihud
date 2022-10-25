package minihud.util.value;

import com.google.common.collect.ImmutableList;

import malilib.config.value.BaseOptionListConfigValue;

public class BlockGridMode extends BaseOptionListConfigValue
{
    public static final BlockGridMode NON_AIR  = new BlockGridMode("non_air",  "minihud.name.block_grid_mode.non_air");
    public static final BlockGridMode ADJACENT = new BlockGridMode("adjacent", "minihud.name.block_grid_mode.adjacent");
    public static final BlockGridMode ALL      = new BlockGridMode("all",      "minihud.name.block_grid_mode.all");

    public static final ImmutableList<BlockGridMode> VALUES = ImmutableList.of(NON_AIR, ADJACENT, ALL);

    private BlockGridMode(String name, String translationKey)
    {
        super(name, translationKey);
    }
}
