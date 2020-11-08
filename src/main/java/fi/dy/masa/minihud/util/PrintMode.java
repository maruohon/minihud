package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseConfigOptionListEntry;
import fi.dy.masa.malilib.config.value.ConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum PrintMode implements ConfigOptionListEntry<PrintMode>
{
    NONE    ("none",    "minihud.label.print_mode.none"),
    FAIL    ("fail",    "minihud.label.print_mode.fail"),
    SUCCESS ("success", "minihud.label.print_mode.success"),
    BOTH    ("both",    "minihud.label.print_mode.both");

    public static final ImmutableList<PrintMode> VALUES = ImmutableList.copyOf(values());

    private final String configString;
    private final String translationKey;

    PrintMode(String configString, String translationKey)
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
    public PrintMode cycle(boolean forward)
    {
        return BaseConfigOptionListEntry.cycleValue(VALUES, this.ordinal(), forward);
    }

    @Override
    public PrintMode fromString(String name)
    {
        return BaseConfigOptionListEntry.findValueByName(name, VALUES);
    }
}
