package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.values.ConfigOptionListEntryBase;

public class ShapeRenderType extends ConfigOptionListEntryBase<ShapeRenderType>
{
    public static final ShapeRenderType FULL_BLOCK = new ShapeRenderType("full_block", "minihud.label.shape_render_type.full_block");
    public static final ShapeRenderType INNER_EDGE = new ShapeRenderType("inner_edge", "minihud.label.shape_render_type.inner_edge");
    public static final ShapeRenderType OUTER_EDGE = new ShapeRenderType("outer_edge", "minihud.label.shape_render_type.outer_edge");

    public static final ImmutableList<ShapeRenderType> VALUES = ImmutableList.of(OUTER_EDGE, INNER_EDGE, FULL_BLOCK);

    static
    {
        ConfigOptionListEntryBase.initValues(VALUES);
    }

    public ShapeRenderType(String configString, String translationKey)
    {
        super(configString, translationKey);
    }
}
