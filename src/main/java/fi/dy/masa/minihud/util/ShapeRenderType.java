package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseOptionListConfigValue;

public class ShapeRenderType extends BaseOptionListConfigValue
{
    public static final ShapeRenderType FULL_BLOCK = new ShapeRenderType("full_block", "minihud.label.shape_render_type.full_block");
    public static final ShapeRenderType INNER_EDGE = new ShapeRenderType("inner_edge", "minihud.label.shape_render_type.inner_edge");
    public static final ShapeRenderType OUTER_EDGE = new ShapeRenderType("outer_edge", "minihud.label.shape_render_type.outer_edge");

    public static final ImmutableList<ShapeRenderType> VALUES = ImmutableList.of(FULL_BLOCK, INNER_EDGE, OUTER_EDGE);

    private ShapeRenderType(String name, String translationKey)
    {
        super(name, translationKey);
    }
}
