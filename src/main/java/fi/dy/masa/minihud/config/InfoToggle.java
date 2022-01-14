package fi.dy.masa.minihud.config;

import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.IConfigInteger;
import fi.dy.masa.malilib.config.IHotkeyTogglable;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBoolean;
import fi.dy.masa.malilib.hotkeys.KeybindMulti;
import fi.dy.masa.malilib.hotkeys.KeybindSettings;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.MiniHUD;

public enum InfoToggle implements IConfigInteger, IHotkeyTogglable
{
    BEE_COUNT               ("infoBeeCount",                false, 36, "", "Show the number of bees in the targeted Hive or Nest.\n§6Note: This only works in single player (without server-side support,\n§6which doesn't exist yet, but will be in the Servux mod at some point)."),
    BIOME                   ("infoBiome",                   false, 19, "", "Show the name of the current biome"),
    BIOME_REG_NAME          ("infoBiomeRegistryName",       false, 20, "", "Show the registry name of the current biome"),
    BLOCK_BREAK_SPEED       ("infoBlockBreakSpeed",         false,  6, "", "Show the player's current block breaking speed\nover the last 100 ticks (5 seconds)"),
    BLOCK_IN_CHUNK          ("infoBlockInChunk",            false, 28, "", "Show the player's current position within the Chunk"),
    BLOCK_POS               ("infoBlockPosition",           false,  6, "", "Show the player's current block position"),
    BLOCK_PROPS             ("infoBlockProperties",         false, 27, "", "Show the BlockState properties and values"),
    CHUNK_POS               ("infoChunkPosition",           false,  7, "", "Show the Chunk the player is currently within"),
    CHUNK_SECTIONS          ("infoChunkSections",           false, 14, "", "Show the currently rendered number of\nChunk sections (the C value from F3)"),
    CHUNK_SECTIONS_FULL     ("infoChunkSectionsLine",       false, 15, "", "Show the entire line of the C value from the F3 screen"),
    CHUNK_UPDATES           ("infoChunkUpdates",            false, 16, "", "Show the current number of chunk updates per second"),
    COORDINATES             ("infoCoordinates",             true,   4, "", "Show the player's coordinates"),
    COORDINATES_SCALED      ("infoCoordinatesScaled",       false,  4, "", "Show the player's coordinates scaled by the dimension's scale factor.\nOnly works in the overworld and the nether,\nand assumes the vanilla nether portal scaling factor."),
    DIFFICULTY              ("infoDifficulty",              false, 18, "", "Show the local difficulty"),
    DIMENSION               ("infoDimensionId",             false,  5, "", "Show the current dimension ID\n(might not be accurate in every case,\ndepending on the server (Sponge?)!)"),
    DISTANCE                ("infoDistance",                false, 33, "", "Show the distance to the current reference point.\nSet the reference point with the setDistanceReferencePoint hotkey"),
    ENTITIES                ("infoEntities",                false, 21, "", "Show the visible/loaded entity count"),
    ENTITIES_CLIENT_WORLD   ("infoEntitiesClientWorld",     false, 22, "", "Show the entity count in the world list/map"),
    ENTITY_REG_NAME         ("infoEntityRegistryName",      false, 24, "", "Show the registry name of the entity\nthe player is currently looking at"),
    FACING                  ("infoFacing",                  true,   8, "", "Show the player's current facing"),
    FURNACE_XP              ("infoFurnaceXp",               false, 30, "", "Show the amount of XP in the looked at Furnace.\n§6Note: This only works in single player (without server-side support,\n§6which doesn't exist yet, but will be in the Servux mod at some point)."),
    FPS                     ("infoFPS",                     false,  0, "", "Show the current FPS"),
    HONEY_LEVEL             ("infoHoneyLevel",              false, 37, "", "Show the honey level the targeted Hive or Nest.\nNote: This only works in single player (without server-side support,\n§6which doesn't exist yet, but will be in the Servux mod at some point)."),
    HORSE_SPEED             ("infoHorseSpeed",              false, 36, "", "Show the speed (m/s) of the horse being ridden."),
    HORSE_JUMP              ("infoHorseJump",               false, 37, "", "Show the jump height of the horse being ridden."),
    LIGHT_LEVEL             ("infoLightLevel",              false, 10, "", "Show the current light level"),
    LOOKING_AT_BLOCK        ("infoLookingAtBlock",          false, 25, "", "Show which block the player is currently looking at"),
    LOOKING_AT_BLOCK_CHUNK  ("infoLookingAtBlockInChunk",   false, 26, "", "Show which block within its containing chunk\nthe player is currently looking at"),
    LOOKING_AT_ENTITY       ("infoLookingAtEntity",         false, 23, "", "Show the entity name and health when looked at"),
    MEMORY_USAGE            ("infoMemoryUsage",             false,  0, "", "Show the memory usage and allocation"),
    LOADED_CHUNKS_COUNT     ("infoLoadedChunksCount",       false, 31, "", "Show the number of loaded chunks on the client"),
    PARTICLE_COUNT          ("infoParticleCount",           false, 17, "", "Show the currently renderer particle count (P from F3)"),
    PING                    ("infoPing",                    false, 36, "", "Show the current ping to the server"),
    REGION_FILE             ("infoRegionFile",              false, 29, "", "Show the region file the player is currently within"),
    ROTATION_PITCH          ("infoRotationPitch",           false, 12, "", "Show the player's pitch rotation"),
    ROTATION_YAW            ("infoRotationYaw",             false, 11, "", "Show the player's yaw rotation"),
    SERVER_TPS              ("infoServerTPS",               false,  9, "", "Show the server TPS and ms/tick (MSPT) values\nNote: This is only accurate when running a Carpet server\nand the TPSdisplay carpet rule is enabled.\nOtherwise it is estimated on the client side,\nbased on the world time update packet, and it can\nonly detect TPS values lower than 20 that way."),
    SLIME_CHUNK             ("infoSlimeChunk",              false, 22, "", "Show whether the player is currently in a slime chunk.\nNOTE: This only works in single player without any user intervention!\nOn a server the player needs to either:\n  1) be admin/OP and run the /seed command manually EVERY TIME\n     they join or change dimensions, or\n  2) input the seed via chat, by sending a \"command\" like: minihud-seed 12345"),
    SPEED                   ("infoSpeed",                   false, 13, "", "Show the player's current moving speed"),
    SPEED_AXIS              ("infoSpeedAxis",               false, 13, "", "Show the player's current moving speed per axis"),
    SPEED_HV                ("infoSpeedHV",                 false, 13, "", "Show the player's current moving speed both horizontally and vertically."),
    SPRINTING               ("infoSprinting",               false, 40, "", "Show the \"Sprinting\" info line if the player is sprinting"),
    TILE_ENTITIES           ("infoTileEntities",            false, 32, "", "Show the number of TileEntities in the client world"),
    TIME_DAY_MODULO         ("infoTimeDayModulo",           false, 35, "", "Show a modulo of the current day time.\nSee Generic configs for the divisor."),
    TIME_REAL               ("infoTimeIRL",                 true,   1, "", "Show the current real time formatted according to dateFormatReal"),
    TIME_TOTAL_MODULO       ("infoTimeTotalModulo",         false, 34, "", "Show a modulo of the current total world time.\nSee Generic configs for the divisor."),
    TIME_WORLD              ("infoTimeWorld",               false,  2, "", "Show the current world time in ticks"),
    TIME_WORLD_FORMATTED    ("infoWorldTimeFormatted",      false,  3, "", "Show the current world time formatted to days, hours, minutes");

    public static final ImmutableList<InfoToggle> VALUES = ImmutableList.copyOf(values());

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
        this(name, defaultValue, linePosition, defaultHotkey, comment, KeybindSettings.DEFAULT);
    }

    private InfoToggle(String name, boolean defaultValue, int linePosition, String defaultHotkey, String comment, KeybindSettings settings)
    {
        this.name = name;
        this.prettyName = name;
        this.valueBoolean = defaultValue;
        this.defaultValueBoolean = defaultValue;
        this.keybind = KeybindMulti.fromStorageString(defaultHotkey, settings);
        this.keybind.setCallback(new KeyCallbackToggleBoolean(this));
        this.linePosition = linePosition;
        this.defaultLinePosition = linePosition;
        this.comment = comment;
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
        return StringUtils.getTranslatedOrFallback("config.comment." + this.getName().toLowerCase(), this.comment);
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
    public int getMinIntegerValue()
    {
        return 0;
    }

    @Override
    public int getMaxIntegerValue()
    {
        return InfoToggle.values().length - 1;
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
            MiniHUD.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
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
                MiniHUD.logger.warn("Failed to read config value for {} from the JSON config", this.getName());
            }
        }
        catch (Exception e)
        {
            MiniHUD.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
        }
    }

    @Override
    public JsonElement getAsJsonElement()
    {
        return new JsonPrimitive(this.valueBoolean);
    }
}
