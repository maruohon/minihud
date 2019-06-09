package fi.dy.masa.minihud.util;

import fi.dy.masa.malilib.config.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum LightLevelMarkerMode implements IConfigOptionListEntry
{
    NONE    ("none",    "minihud.label.light_level_marker_mode.none"),
    CROSS   ("cross",   "minihud.label.light_level_marker_mode.cross"),
    SQUARE  ("square",  "minihud.label.light_level_marker_mode.square");

    private final String configString;
    private final String translationKey;

    private LightLevelMarkerMode(String configString, String translationKey)
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
    public IConfigOptionListEntry cycle(boolean forward)
    {
        int id = this.ordinal();

        if (forward)
        {
            if (++id >= values().length)
            {
                id = 0;
            }
        }
        else
        {
            if (--id < 0)
            {
                id = values().length - 1;
            }
        }

        return values()[id % values().length];
    }

    @Override
    public LightLevelMarkerMode fromString(String name)
    {
        return fromStringStatic(name);
    }

    public static LightLevelMarkerMode fromStringStatic(String name)
    {
        for (LightLevelMarkerMode val : LightLevelMarkerMode.values())
        {
            if (val.configString.equalsIgnoreCase(name))
            {
                return val;
            }
        }

        return LightLevelMarkerMode.NONE;
    }
}
