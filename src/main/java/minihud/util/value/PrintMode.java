package minihud.util.value;

import com.google.common.collect.ImmutableList;

import malilib.config.value.BaseOptionListConfigValue;

public class PrintMode extends BaseOptionListConfigValue
{
    public static final PrintMode NONE    = new PrintMode("none",    "minihud.name.print_mode.none");
    public static final PrintMode FAIL    = new PrintMode("fail",    "minihud.name.print_mode.fail");
    public static final PrintMode SUCCESS = new PrintMode("success", "minihud.name.print_mode.success");
    public static final PrintMode BOTH    = new PrintMode("both",    "minihud.name.print_mode.both");

    public static final ImmutableList<PrintMode> VALUES = ImmutableList.of(NONE, FAIL, SUCCESS, BOTH);

    private PrintMode(String name, String translationKey)
    {
        super(name, translationKey);
    }
}
