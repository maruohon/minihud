package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.ConfigOptionListEntry;
import fi.dy.masa.malilib.config.value.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum BlockGridMode implements IConfigOptionListEntry<BlockGridMode>
{
    ALL         ("all",         "minihud.label.blockgridmode.all"),
    NON_AIR     ("non_air",     "minihud.label.blockgridmode.non_air"),
    ADJACENT    ("adjacent",    "minihud.label.blockgridmode.adjacent");

    public static final ImmutableList<BlockGridMode> VALUES = ImmutableList.copyOf(values());

    private final String configString;
    private final String translationKey;

    BlockGridMode(String configString, String translationKey)
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
    public BlockGridMode cycle(boolean forward)
    {
        return ConfigOptionListEntry.cycleValue(VALUES, this.ordinal(), forward);
    }

    @Override
    public BlockGridMode fromString(String name)
    {
        return ConfigOptionListEntry.findValueByName(name, VALUES);
    }
}
