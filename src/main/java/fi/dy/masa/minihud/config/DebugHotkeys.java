package fi.dy.masa.minihud.config;

import javax.annotation.Nullable;
import fi.dy.masa.minihud.config.Configs.ConfigType;
import fi.dy.masa.minihud.config.interfaces.IConfig;
import fi.dy.masa.minihud.config.interfaces.IConfigHotkey;
import fi.dy.masa.minihud.event.InputEventHandler;

public enum DebugHotkeys implements IConfig, IConfigHotkey
{
    COLLISION_BOXES     ("debugCollisionBoxEnabled",    InputEventHandler.MASK_DEBUG_COLLISION_BOXES,   "1"),
    HEIGHT_MAP          ("debugHeightMapEnabled",       InputEventHandler.MASK_DEBUG_HEIGHT_MAP,        "2"),
    NEIGHBOR_UPDATES    ("debugNeighborsUpdateEnabled", InputEventHandler.MASK_DEBUG_NEIGHBOR_UPDATE,   "3"),
    PATH_FINDING        ("debugPathfindingEnabled",     InputEventHandler.MASK_DEBUG_PATHFINDING,       "4"),
    SOLID_FACES         ("debugSolidFaceEnabled",       InputEventHandler.MASK_DEBUG_SOLID_FACES,       "5"),
    WATER               ("debugWaterEnabled",           InputEventHandler.MASK_DEBUG_WATER,             "6");

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
