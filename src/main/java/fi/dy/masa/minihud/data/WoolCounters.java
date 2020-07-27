package fi.dy.masa.minihud.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.minecraft.item.EnumDyeColor;
import fi.dy.masa.malilib.gui.GuiBase;
import fi.dy.masa.malilib.util.data.DyeColorCode;
import fi.dy.masa.minihud.config.Configs;

public class WoolCounters
{
    private static final EnumDyeColor[] COLORS = EnumDyeColor.values();

    private final long[] counters = new long[COLORS.length];
    private final boolean[] enabledCounters = new boolean[COLORS.length];

    public void clear()
    {
        Arrays.fill(this.counters, -1);
    }

    public boolean isEnabled(EnumDyeColor color)
    {
        return this.enabledCounters[color.getMetadata()];
    }

    public void setValue(EnumDyeColor color, long value)
    {
        this.counters[color.getMetadata()] = value;
    }

    public List<String> getInfoLines()
    {
        ArrayList<String> lines = new ArrayList<String>();

        for (int meta = 0; meta < this.counters.length; ++meta)
        {
            long value = this.counters[meta];

            if (value != -1 && (this.enabledCounters[meta]
                || Configs.Generic.WOOL_COUNTER_ENABLE_ALL.getBooleanValue()))
            {
                EnumDyeColor color = EnumDyeColor.byMetadata(meta);

                lines.add(String.format("Wool Counter [ %s%s%s ]: %s%d%s",
                        DyeColorCode.getByMeta(meta).getTextColor().toString(), color.getName(), GuiBase.TXT_RST,
                        GuiBase.TXT_AQUA, value, GuiBase.TXT_RST));
            }
        }

        return lines;
    }

    public void updateEnabledCounters(String configStr)
    {
        String[] parts = configStr.split(",");
        Pattern patternRange = Pattern.compile("^(?<start>[0-9]+)-(?<end>[0-9]+)$");
        Pattern patternMeta = Pattern.compile("^[0-9]+$");
        Pattern patternName = Pattern.compile("^[a-zA-Z_]+$");

        Arrays.fill(this.enabledCounters, false);

        for (String str : parts)
        {
            try
            {
                Matcher matcher = patternRange.matcher(str);

                if (matcher.matches())
                {
                    int start = Integer.parseInt(matcher.group("start"));
                    int end = Integer.parseInt(matcher.group("end"));

                    if (start <= end && start >= 0 && end <= 15)
                    {
                        for (int meta = start; meta <= end; ++meta)
                        {
                            this.enabledCounters[meta] = true;
                        }
                    }
                }

                matcher = patternMeta.matcher(str);

                if (matcher.matches())
                {
                    int meta = Integer.parseInt(str);

                    if (meta >= 0 && meta <= 15)
                    {
                        this.enabledCounters[meta] = true;
                    }
                }

                matcher = patternName.matcher(str);

                if (matcher.matches())
                {
                    for (EnumDyeColor color : COLORS)
                    {
                        if (color.getName().equalsIgnoreCase(str))
                        {
                            this.enabledCounters[color.getMetadata()] = true;
                            break;
                        }
                    }
                }
            }
            catch (Exception e)
            {
            }
        }
    }
}
