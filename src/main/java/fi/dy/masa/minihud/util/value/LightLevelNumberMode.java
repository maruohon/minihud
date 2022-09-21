package fi.dy.masa.minihud.util.value;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseOptionListConfigValue;

public class LightLevelNumberMode extends BaseOptionListConfigValue
{
    public static final LightLevelNumberMode NONE  = new LightLevelNumberMode("none",  "minihud.name.light_level_number_mode.none");
    public static final LightLevelNumberMode BLOCK = new LightLevelNumberMode("block", "minihud.name.light_level_number_mode.block");
    public static final LightLevelNumberMode SKY   = new LightLevelNumberMode("sky",   "minihud.name.light_level_number_mode.sky");
    public static final LightLevelNumberMode BOTH  = new LightLevelNumberMode("both",  "minihud.name.light_level_number_mode.both");

    public static final ImmutableList<LightLevelNumberMode> VALUES = ImmutableList.of(BLOCK, SKY, BOTH, NONE);

    private LightLevelNumberMode(String name, String translationKey)
    {
        super(name, translationKey);
    }
}
