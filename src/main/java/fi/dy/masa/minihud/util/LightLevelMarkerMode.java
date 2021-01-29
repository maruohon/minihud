package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.value.BaseOptionListConfigValue;

public class LightLevelMarkerMode extends BaseOptionListConfigValue
{
    public static final LightLevelMarkerMode SQUARE = new LightLevelMarkerMode("square",  "minihud.label.light_level_marker_mode.square");
    public static final LightLevelMarkerMode CROSS  = new LightLevelMarkerMode("cross",   "minihud.label.light_level_marker_mode.cross");
    public static final LightLevelMarkerMode NONE   = new LightLevelMarkerMode("none",    "minihud.label.light_level_marker_mode.none");

    public static final ImmutableList<LightLevelMarkerMode> VALUES = ImmutableList.of(SQUARE, CROSS, NONE);

    private LightLevelMarkerMode(String name, String translationKey)
    {
        super(name, translationKey);
    }
}
