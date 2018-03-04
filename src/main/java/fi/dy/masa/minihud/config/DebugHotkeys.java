package fi.dy.masa.minihud.config;

import javax.annotation.Nullable;
import fi.dy.masa.minihud.config.interfaces.ConfigType;
import fi.dy.masa.minihud.config.interfaces.IConfig;
import fi.dy.masa.minihud.config.interfaces.IConfigHotkey;

public enum DebugHotkeys implements IConfig, IConfigHotkey
{
    COLLISION_BOXES     ("debugCollisionBoxEnabled",    0x01, "1"),
    HEIGHT_MAP          ("debugHeightMapEnabled",       0x02, "2"),
    NEIGHBOR_UPDATES    ("debugNeighborsUpdateEnabled", 0x04, "3"),
    PATH_FINDING        ("debugPathfindingEnabled",     0x08, "4"),
    SOLID_FACES         ("debugSolidFaceEnabled",       0x10, "5"),
    WATER               ("debugWaterEnabled",           0x20, "6");

    private final String name;
    private final int bitMask;
    private String hotkey;

    private DebugHotkeys(String name, int bitMask, String defaultHotkey)
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
