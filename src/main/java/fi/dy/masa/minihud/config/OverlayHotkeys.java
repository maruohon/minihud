package fi.dy.masa.minihud.config;

import javax.annotation.Nullable;
import fi.dy.masa.minihud.config.interfaces.ConfigType;
import fi.dy.masa.minihud.config.interfaces.IConfig;
import fi.dy.masa.minihud.config.interfaces.IConfigHotkey;

public enum OverlayHotkeys implements IConfig, IConfigHotkey
{
    REGION_FILE         ("regionFileOverlay",           0x01, "J");

    private final String name;
    private final int bitMask;
    private String hotkey;

    private OverlayHotkeys(String name, int bitMask, String defaultHotkey)
    {
        this.name = name;
        this.bitMask = bitMask;
        this.hotkey = defaultHotkey;
    }

    @Override
    public ConfigType getType()
    {
        return ConfigType.HOTKEY;
    }

    @Override
    public String getName()
    {
        return this.name;
    }

    @Override
    @Nullable
    public String getComment()
    {
        return null;
    }

    @Override
    public String getStringValue()
    {
        return this.hotkey;
    }

    public int getBitMask()
    {
        return this.bitMask;
    }

    @Override
    public String getHotkey()
    {
        return this.hotkey;
    }

    @Override
    public void setHotkey(String hotkey)
    {
        this.hotkey = hotkey;
    }
}
