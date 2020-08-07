package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseConfigOptionListEntry;
import fi.dy.masa.malilib.config.value.ConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum LightLevelNumberMode implements ConfigOptionListEntry<LightLevelNumberMode>
{
    NONE    ("none",    "minihud.label.light_level_number_mode.none"),
    BLOCK   ("block",   "minihud.label.light_level_number_mode.block"),
    SKY     ("sky",     "minihud.label.light_level_number_mode.sky"),
    BOTH    ("both",    "minihud.label.light_level_number_mode.both");

    public static final ImmutableList<LightLevelNumberMode> VALUES = ImmutableList.copyOf(values());

    private final String configString;
    private final String translationKey;

    LightLevelNumberMode(String configString, String translationKey)
    {
        this.configString = configString;
        this.translationKey = translationKey;
    }

    @Override
    public String getStringValue()
    {
        return this.configString;
    }

    @Override
    public String getDisplayName()
    {
        return StringUtils.translate(this.translationKey);
    }

    @Override
    public LightLevelNumberMode cycle(boolean forward)
    {
        return BaseConfigOptionListEntry.cycleValue(VALUES, this.ordinal(), forward);
    }

    @Override
    public LightLevelNumberMode fromString(String name)
    {
        return BaseConfigOptionListEntry.findValueByName(name, VALUES);
    }
}
