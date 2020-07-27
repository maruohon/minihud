package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.ConfigOptionListEntry;
import fi.dy.masa.malilib.config.value.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum ShapeRenderType implements IConfigOptionListEntry<ShapeRenderType>
{
    FULL_BLOCK ("full_block", "minihud.label.shape_render_type.full_block"),
    INNER_EDGE ("inner_edge", "minihud.label.shape_render_type.inner_edge"),
    OUTER_EDGE ("outer_edge", "minihud.label.shape_render_type.outer_edge");

    public static final ImmutableList<ShapeRenderType> VALUES = ImmutableList.copyOf(values());

    private final String configString;
    private final String translationKey;

    ShapeRenderType(String configString, String translationKey)
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
    public ShapeRenderType cycle(boolean forward)
    {
        return ConfigOptionListEntry.cycleValue(VALUES, this.ordinal(), forward);
    }

    @Override
    public ShapeRenderType fromString(String name)
    {
        return ConfigOptionListEntry.findValueByName(name, VALUES);
    }
}
