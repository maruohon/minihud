package fi.dy.masa.minihud.util;

import fi.dy.masa.malilib.config.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum ShapeRenderType implements IConfigOptionListEntry
{
    FULL_BLOCK  ("full_block", "minihud.label.shape_render_type.full_block"),
    INNER_EDGE ("inner_edge", "minihud.label.shape_render_type.inner_edge"),
    OUTER_EDGE ("outer_edge", "minihud.label.shape_render_type.outer_edge");

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
    public ShapeRenderType fromString(String name)
    {
        return fromStringStatic(name);
    }

    public static ShapeRenderType fromStringStatic(String name)
    {
        for (ShapeRenderType val : ShapeRenderType.values())
        {
            if (val.configString.equalsIgnoreCase(name))
            {
                return val;
            }
        }

        return ShapeRenderType.OUTER_EDGE;
    }
}
