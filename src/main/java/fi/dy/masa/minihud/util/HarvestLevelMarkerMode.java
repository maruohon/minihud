package fi.dy.masa.minihud.util;

import fi.dy.masa.malilib.config.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum HarvestLevelMarkerMode implements IConfigOptionListEntry
{
    NONE    ("none",    "minihud.label.harvest_level_marker_mode.none"),
    CROSS   ("cross",   "minihud.label.harvest_level_marker_mode.cross"),
    SQUARE  ("square",  "minihud.label.harvest_level_marker_mode.square");

    private final String configString;
    private final String translationKey;

    private HarvestLevelMarkerMode(String configString, String translationKey)
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
    public HarvestLevelMarkerMode fromString(String name)
    {
        return fromStringStatic(name);
    }

    public static HarvestLevelMarkerMode fromStringStatic(String name)
    {
        for (HarvestLevelMarkerMode val : HarvestLevelMarkerMode.values())
        {
            if (val.configString.equalsIgnoreCase(name))
            {
                return val;
            }
        }

        return HarvestLevelMarkerMode.NONE;
    }
}
