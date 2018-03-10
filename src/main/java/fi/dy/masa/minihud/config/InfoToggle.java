package fi.dy.masa.minihud.config;

import fi.dy.masa.minihud.config.interfaces.ConfigType;
import fi.dy.masa.minihud.config.interfaces.IConfigBoolean;
import fi.dy.masa.minihud.config.interfaces.IConfigHotkey;

// Note: the bitmask affects the default ordering of the info lines!
public enum InfoToggle implements IConfigBoolean, IConfigHotkey
{
    BIOME                   ("infoBiome",                   false, 0x00040000, "B", "Show the name of the current biome"),
    BIOME_REG_NAME          ("infoBiomeRegistryName",       false, 0x00080000, "B", "Show the registry name of the current biome"),
    BLOCK_IN_CHUNK          ("infoBlockInChunk",            false, 0x08000000, "",  "Show the player's current position within the Chunk"),
    BLOCK_POS               ("infoBlockPosition",           false, 0x00000040, "K", "Show the player's current block position"),
    BLOCK_PROPS             ("infoBlockProperties",         false, 0x04000000, "P", "Show the BlockState properties and values"),
    CHUNK_POS               ("infoChunkPosition",           false, 0x00000080, "C", "Show the Chunk the player is currently within"),
    CHUNK_SECTIONS          ("infoChunkSections",           false, 0x00002000, "C", "Show the currently rendered number of Chunk sections (the C value from F3)"),
    CHUNK_SECTIONS_FULL     ("infoChunkSectionsLine",       false, 0x00004000, "",  "Show the entire line of the C value from the F3 screen"),
    CHUNK_UNLOAD_ORDER      ("infoChunkUnloadOrder",        false, 0x20000000, "U", "Show the \"unload order\" (HashSet bucket)\nof the current chunk (for vanilla Perma-Loaders)"),
    CHUNK_UPDATES           ("infoChunkUpdates",            false, 0x00008000, "U", "Show the current number of chunk updates per second"),
    COORDINATES             ("infoCoordinates",             true,  0x00000010, "O", "Show the player's coordinates"),
    DIFFICULTY              ("infoDifficulty",              false, 0x00020000, "D", "Show the local difficulty"),
    DIMENSION               ("infoDimensionId",             true,  0x00000020, "I", "Show the current dimension ID\n(might not be accurate in every case, depending on the server (Sponge?)!)"),
    ENTITIES                ("infoEntities",                false, 0x00100000, "E", "Show the visible/loaded entity count"),
    ENTITY_REG_NAME         ("infoEntityRegistryName",      false, 0x00800000, "E", "Show the registry name of the entity the player is currently looking at"),
    FACING                  ("infoFacing",                  true,  0x00000100, "F", "Show the player's current facing"),
    FPS                     ("infoFPS",                     false, 0x00000001, "",  "Show the current FPS"),
    LIGHT_LEVEL             ("infoLightLevel",              false, 0x00000200, "L", "Show the current light level"),
    LOOKING_AT_BLOCK        ("infoLookingAtBlock",          false, 0x01000000, "A", "Show which block the player is currently looking at"),
    LOOKING_AT_BLOCK_CHUNK  ("infoLookingAtBlockInChunk",   false, 0x02000000, "",  "Show which block within its containing chunk the player is currently looking at"),
    LOOKING_AT_ENTITY       ("infoLookingAtEntity",         false, 0x00400000, "A", "Show the entity name and health when looked at"),
    PARTICLE_COUNT          ("infoParticleCount",           false, 0x00010000, "",  "Show the currently renderer particle count (P from F3)"),
    TIME_REAL               ("infoTimeIRL",                 true,  0x00000002, "T", "Show the current real time formatted according to dateFormatReal"),
    TIME_WORLD              ("infoTimeWorld",               false, 0x00000004, "",  "Show the current world time in ticks"),
    TIME_WORLD_FORMATTED    ("infoWorldTimeFormatted",      false, 0x00000008, "",  "Show the current world time formatted to days, hours, minutes"),
    REGION_FILE             ("infoRegionFile",              false, 0x10000000, "G", "Show the region file the player is currently within"),
    ROTATION_PITCH          ("infoRotationPitch",           false, 0x00000800, "R", "Show the player's pitch rotation"),
    ROTATION_YAW            ("infoRotationYaw",             false, 0x00000400, "R", "Show the player's yaw rotation"),
    SLIME_CHUNK             ("infoSlimeChunk",              false, 0x00200000, "M",
                            "Show whether the player is currently in a slime chunk.\n" +
                            "NOTE: This only works in single player without any user intervention!\n" +
                            "On a server the player needs to be admin/OP and\n" +
                            "run the /seed command manually EVERY TIME they join or change dimensions!"),
    SPEED                   ("infoSpeed",                   false, 0x00001000, "S", "Show the player's current moving speed");

    private final String name;
    private final String comment;
    private final int bitMask;
    private boolean valueBoolean;
    private String hotkey;
    private int linePosition = -1;

    private InfoToggle(String name, boolean defaultValue, int bitMask, String defaultHotkey, String comment)
    {
        this.name = name;
        this.valueBoolean = defaultValue;
        this.bitMask = bitMask;
        this.hotkey = defaultHotkey;
        this.comment = comment;
    }

    @Override
    public ConfigType getType()
    {
        return ConfigType.BOOLEAN;
    }

    @Override
    public String getName()
    {
        return this.name;
    }

    @Override
    public String getStringValue()
    {
        return String.valueOf(this.valueBoolean);
    }

    @Override
    public String getComment()
    {
        return comment != null ? comment : "";
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

    public int getLinePosition()
    {
        return this.linePosition;
    }

    public void setLinePosition(int position)
    {
        this.linePosition = position;
    }

    public int applyBitMask(int mask)
    {
        if (this.valueBoolean)
        {
            mask |= this.bitMask;
        }
        else
        {
            mask &= ~this.bitMask;
        }

        return mask;
    }

    @Override
    public boolean getBooleanValue()
    {
        return this.valueBoolean;
    }

    @Override
    public void setBooleanValue(boolean value)
    {
        this.valueBoolean = value;
    }
}
