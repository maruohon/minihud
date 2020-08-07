package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseConfigOptionListEntry;
import fi.dy.masa.malilib.config.value.ConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum LightLevelMarkerMode implements ConfigOptionListEntry<LightLevelMarkerMode>
{
    NONE    ("none",    "minihud.label.light_level_marker_mode.none"),
    CROSS   ("cross",   "minihud.label.light_level_marker_mode.cross"),
    SQUARE  ("square",  "minihud.label.light_level_marker_mode.square");

    public static final ImmutableList<LightLevelMarkerMode> VALUES = ImmutableList.copyOf(values());

    private final String configString;
    private final String translationKey;

    LightLevelMarkerMode(String configString, String translationKey)
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
    public LightLevelMarkerMode cycle(boolean forward)
    {
        return BaseConfigOptionListEntry.cycleValue(VALUES, this.ordinal(), forward);
    }

    @Override
    public LightLevelMarkerMode fromString(String name)
    {
        return BaseConfigOptionListEntry.findValueByName(name, VALUES);
    }
}
