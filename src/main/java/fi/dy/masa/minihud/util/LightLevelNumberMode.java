package fi.dy.masa.minihud.util;

import fi.dy.masa.malilib.config.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum LightLevelNumberMode implements IConfigOptionListEntry
{
    NONE    ("none",    "minihud.label.light_level_number_mode.none"),
    BLOCK   ("block",   "minihud.label.light_level_number_mode.block"),
    SKY     ("sky",     "minihud.label.light_level_number_mode.sky"),
    BOTH    ("both",    "minihud.label.light_level_number_mode.both");

    private final String configString;
    private final String translationKey;

    private LightLevelNumberMode(String configString, String translationKey)
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
    public LightLevelNumberMode fromString(String name)
    {
        return fromStringStatic(name);
    }

    public static LightLevelNumberMode fromStringStatic(String name)
    {
        for (LightLevelNumberMode val : LightLevelNumberMode.values())
        {
            if (val.configString.equalsIgnoreCase(name))
            {
                return val;
            }
        }

        return LightLevelNumberMode.NONE;
    }
}
