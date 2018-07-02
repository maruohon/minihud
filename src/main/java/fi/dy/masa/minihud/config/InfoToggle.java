package fi.dy.masa.minihud.config;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.config.IConfigInteger;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeybindMulti;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleInfoLine;

public enum InfoToggle implements IConfigBoolean, IConfigInteger, IHotkey
{
    BIOME                   ("infoBiome",                   false, 19, "H,I", "Show the name of the current biome"),
    BIOME_REG_NAME          ("infoBiomeRegistryName",       false, 20, "H,I", "Show the registry name of the current biome"),
    BLOCK_IN_CHUNK          ("infoBlockInChunk",            false, 28, "H,K", "Show the player's current position within the Chunk"),
    BLOCK_POS               ("infoBlockPosition",           false,  6, "H,B", "Show the player's current block position"),
    BLOCK_PROPS             ("infoBlockProperties",         false, 27, "H,P", "Show the BlockState properties and values"),
    CHUNK_POS               ("infoChunkPosition",           false,  7, "H,C", "Show the Chunk the player is currently within"),
    CHUNK_SECTIONS          ("infoChunkSections",           false, 14, "H,C", "Show the currently rendered number of Chunk sections (the C value from F3)"),
    CHUNK_SECTIONS_FULL     ("infoChunkSectionsLine",       false, 15, "",    "Show the entire line of the C value from the F3 screen"),
    CHUNK_UNLOAD_ORDER      ("infoChunkUnloadOrder",        false, 30, "H,U", "Show the \"unload order\" (HashSet bucket)\nof the current chunk (for vanilla Perma-Loaders)"),
    CHUNK_UPDATES           ("infoChunkUpdates",            false, 16, "H,U", "Show the current number of chunk updates per second"),
    COORDINATES             ("infoCoordinates",             true,   4, "H,O", "Show the player's coordinates"),
    DIFFICULTY              ("infoDifficulty",              false, 18, "H,D", "Show the local difficulty"),
    DIMENSION               ("infoDimensionId",             false,  5, "",    "Show the current dimension ID\n(might not be accurate in every case, depending on the server (Sponge?)!)"),
    ENTITIES                ("infoEntities",                false, 21, "H,E", "Show the visible/loaded entity count"),
    ENTITY_REG_NAME         ("infoEntityRegistryName",      false, 24, "H,E", "Show the registry name of the entity the player is currently looking at"),
    FACING                  ("infoFacing",                  true,   8, "H,F", "Show the player's current facing"),
    FPS                     ("infoFPS",                     false,  0, "",    "Show the current FPS"),
    LIGHT_LEVEL             ("infoLightLevel",              false, 10, "H,L", "Show the current light level"),
    LOOKING_AT_BLOCK        ("infoLookingAtBlock",          false, 25, "H,A", "Show which block the player is currently looking at"),
    LOOKING_AT_BLOCK_CHUNK  ("infoLookingAtBlockInChunk",   false, 26, "",    "Show which block within its containing chunk the player is currently looking at"),
    LOOKING_AT_ENTITY       ("infoLookingAtEntity",         false, 23, "H,A", "Show the entity name and health when looked at"),
    MEMORY_USAGE            ("infoMemoryUsage",             false,  0, "",    "Show the memory usage and allocation"),
    MP_CHUNK_CACHE          ("infoMultiplayerChunkCache",   false, 31, "",    "Show the Multiplayer Chunk Cache size"),
    PARTICLE_COUNT          ("infoParticleCount",           false, 17, "",    "Show the currently renderer particle count (P from F3)"),
    SERVER_TPS              ("infoServerTPS",               false,  9, "",    "Show the server TPS and ms/tick (MSPT) values\nNote: This is only accurate when running a Carpet server\nand the TPSdisplay carpet rule is enabled.\nOtherwise it is estimated on the client side, based on the world time update packet,\nand it can only detect TPS values lower than 20 that way."),
    SPAWNABLE_SUB_CHUNKS    ("infoSpawnableSubChunks",      false, 32, "",    "Show the spawnable sub chunk count for the current chunk"),
    TIME_REAL               ("infoTimeIRL",                 true,   1, "H,T", "Show the current real time formatted according to dateFormatReal"),
    TIME_WORLD              ("infoTimeWorld",               false,  2, "",    "Show the current world time in ticks"),
    TIME_WORLD_FORMATTED    ("infoWorldTimeFormatted",      false,  3, "",    "Show the current world time formatted to days, hours, minutes"),
    REGION_FILE             ("infoRegionFile",              false, 29, "H,G", "Show the region file the player is currently within"),
    ROTATION_PITCH          ("infoRotationPitch",           false, 12, "H,R", "Show the player's pitch rotation"),
    ROTATION_YAW            ("infoRotationYaw",             false, 11, "H,R", "Show the player's yaw rotation"),
    SLIME_CHUNK             ("infoSlimeChunk",              false, 22, "H,M", "Show whether the player is currently in a slime chunk.\nNOTE: This only works in single player without any user intervention!\nOn a server the player needs to either:\n  1) be admin/OP and run the /seed command manually EVERY TIME\n     they join or change dimensions, or\n  2) input the seed via chat, by sending a \"command\" like: minihud-seed 12345"),
    SPEED                   ("infoSpeed",                   false, 13, "H,S", "Show the player's current moving speed");

    private final String name;
    private final String prettyName;
    private final String comment;
    private final IKeybind keybind;
    private final boolean defaultValueBoolean;
    private final int defaultLinePosition;
    private boolean valueBoolean;
    private int linePosition;

    private InfoToggle(String name, boolean defaultValue, int linePosition, String defaultHotkey, String comment)
    {
        this.name = name;
        this.prettyName = name;
        this.valueBoolean = defaultValue;
        this.defaultValueBoolean = defaultValue;
        this.keybind = KeybindMulti.fromStorageString(defaultHotkey);
        this.keybind.setCallback(new KeyCallbackToggleInfoLine(this));
        this.linePosition = linePosition;
        this.defaultLinePosition = linePosition;
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
    public String getPrettyName()
    {
        return this.prettyName;
    }

    @Override
    public String getStringValue()
    {
        return String.valueOf(this.valueBoolean);
    }

    @Override
    public String getDefaultStringValue()
    {
        return String.valueOf(this.defaultValueBoolean);
    }

    @Override
    public String getComment()
    {
        return comment != null ? this.comment : "";
    }

    @Override
    public boolean getBooleanValue()
    {
        return this.valueBoolean;
    }

    @Override
    public boolean getDefaultBooleanValue()
    {
        return this.defaultValueBoolean;
    }

    @Override
    public void setBooleanValue(boolean value)
    {
        this.valueBoolean = value;
    }

    @Override
    public int getIntegerValue()
    {
        return this.linePosition;
    }

    @Override
    public int getDefaultIntegerValue()
    {
        return this.defaultLinePosition;
    }

    @Override
    public void setIntegerValue(int value)
    {
        this.linePosition = value;
    }

    @Override
    public IKeybind getKeybind()
    {
        return this.keybind;
    }

    @Override
    public boolean isModified()
    {
        return this.valueBoolean != this.defaultValueBoolean;
    }

    @Override
    public boolean isModified(String newValue)
    {
        return String.valueOf(this.defaultValueBoolean).equals(newValue) == false;
    }

    @Override
    public void resetToDefault()
    {
        this.valueBoolean = this.defaultValueBoolean;
    }

    @Override
    public void setValueFromString(String value)
    {
        try
        {
            this.valueBoolean = Boolean.parseBoolean(value);
        }
        catch (Exception e)
        {
            LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
        }
    }

    @Override
    public void setValueFromJsonElement(JsonElement element)
    {
        try
        {
            if (element.isJsonPrimitive())
            {
                this.valueBoolean = element.getAsBoolean();
            }
            else
            {
                LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName());
            }
        }
        catch (Exception e)
        {
            LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
        }
    }

    @Override
    public JsonElement getAsJsonElement()
    {
        return new JsonPrimitive(this.valueBoolean);
    }
}
