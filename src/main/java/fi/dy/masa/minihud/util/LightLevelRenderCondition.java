package fi.dy.masa.minihud.util;

import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.IConfigOptionListEntry;
import fi.dy.masa.malilib.util.StringUtils;

public enum LightLevelRenderCondition implements IConfigOptionListEntry
{
    ALWAYS      ("always",    "minihud.label.light_level_render_condition.always",    (b, d, s) -> true),
    NEVER       ("never",     "minihud.label.light_level_render_condition.never",     (b, d, s) -> false),
    SAFE        ("safe",      "minihud.label.light_level_render_condition.safe",      (b, d, s) -> b >= s && (d <= s || b > d)),
    DIM         ("dim",       "minihud.label.light_level_render_condition.dim",       (b, d, s) -> b <= d && d > s),
    SPAWNABLE   ("spawnable", "minihud.label.light_level_render_condition.spawnable", (b, d, s) -> b < s);

    private static final ImmutableList<LightLevelRenderCondition> VALUES = ImmutableList.copyOf(values());

    private final String configString;
    private final String translationKey;
    private final Condition condition;

    LightLevelRenderCondition(String configString, String translationKey, Condition condition)
    {
        this.configString = configString;
        this.translationKey = translationKey;
        this.condition = condition;
    }

    public boolean shouldRender(int blockLightLevel, int dimThreshold, int safeThreshold)
    {
        return this.condition.shouldRender(blockLightLevel, dimThreshold, safeThreshold);
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
    public LightLevelRenderCondition fromString(String name)
    {
        return fromStringStatic(name);
    }

    public static LightLevelRenderCondition fromStringStatic(String name)
    {
        for (LightLevelRenderCondition val : LightLevelRenderCondition.VALUES)
        {
            if (val.configString.equalsIgnoreCase(name))
            {
                return val;
            }
        }

        return LightLevelRenderCondition.ALWAYS;
    }

    private interface Condition
    {
        boolean shouldRender(int blockLightLevel, int dimThreshold, int safeThreshold);
    }
}
