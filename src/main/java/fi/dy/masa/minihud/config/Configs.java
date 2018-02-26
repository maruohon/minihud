package fi.dy.masa.minihud.config;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.Nullable;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mumfrey.liteloader.core.LiteLoader;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.InputEventHandler;
import fi.dy.masa.minihud.event.InputEventHandler.KeyModifier;
import fi.dy.masa.minihud.event.RenderEventHandler;
import fi.dy.masa.minihud.util.JsonUtils;

public class Configs
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";

    public static int enabledInfoTypes;
    public static KeyModifier requiredKey;
    private static final Multimap<Integer, Integer> HOTKEY_DEBUG_MAP = HashMultimap.create();
    private static final Multimap<Integer, Integer> HOTKEY_INFO_MAP = HashMultimap.create();
    private static final Map<Integer, Integer> LINE_ORDER_MAP = new HashMap<Integer, Integer>();

    public static final String CATEGORY_DEBUG_HOTKEYS = "VanillaDebugRendererHotkeys";
    public static final String CATEGORY_DEBUG_RENDERER = "VanillaDebugRendererOptions";
    public static final String CATEGORY_GENERIC = "Generic";
    public static final String CATEGORY_INFO_TOGGLE = "InfoTypes";
    public static final String CATEGORY_INFO_HOTKEYS = "InfoToggleHotkeys";
    public static final String CATEGORY_INFO_LINE_ORDER = "InfoLineOrder";

    public enum ConfigType
    {
        BOOLEAN,
        INTEGER,
        STRING,
        HEX_STRING,
        HOTKEY;
    }

    public enum Generic implements IConfigGeneric, IConfigBoolean
    {
        COORDINATE_FORMAT_STRING        ("coordinateFormat", "x: %.1f y: %.1f z: %.1f",
                                        "The format string for the coordinate line.\n" +
                                        "Needs to have three %f format strings! Default: x: %.1f y: %.1f z: %.1f"),
        DATE_FORMAT_REAL                ("dateFormatReal", "yyyy-MM-dd HH:mm:ss",
                                        "The format string for real time, see the Java SimpleDateFormat\n" +
                                        "class for the format patterns, if needed."),
        DATE_FORMAT_MINECRAFT           ("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx",
                                        "The format string for the Minecraft time.\n" +
                                        "The supported placeholders are: {DAY}, {HOUR}, {MIN}, {SEC}"),
        DEBUG_RENDERER_PATH_MAX_DIST    ("debugRendererPathfindingEnableMaxDistance", false,
                                        "If true, then the vanilla pathfinding debug renderer will render the max distance boxes.\n" +
                                        "Those obstruct most other things quite badly when enabled, so this is disabled by default."),
        ENABLE_BY_DEFAULT               ("enableByDefault", true, "If true, the HUD will be enabled by default on game launch"),
        FIX_VANILLA_DEBUG_RENDERERS     ("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed."),
        FONT_COLOR                      ("fontColor", "0xE0E0E0", true, "Font color (RGB, default: 0xE0E0E0 = 14737632)"),
        REQUIRE_SNEAK                   ("requireSneak", false, "Require the player to be sneaking to render the HUD"),
        REQUIRE_HOLDING_KEY             ("requireHoldingKey", "none", "Require holding a key to render the HUD. Valid keys are 'alt', 'ctrl' and 'shift'."),
        SORT_LINES_BY_LENGTH            ("sortLinesByLength", false, "Sort the lines by their text's length"),
        SORT_LINES_REVERSED             ("sortLinesReversed", false, "Reverse the line sorting order"),
        TEXT_BACKGROUND_COLOR           ("textBackgroundColor", "0xA0505050", true, "Text background color (ARGB, default: 0xA0505050)"),
        TEXT_POS_X                      ("textPosX", 4, "Text X position from the screen edge (default: 4)"),
        TEXT_POS_Y                      ("textPosY", 4, "Text Y position from the screen edge (default: 4)"),
        USE_CUSTOMIZED_COORDINATES      ("useCustomizedCoordinateFormat", true, "Use the customized coordinate format string"),
        USE_FONT_SHADOW                 ("useFontShadow", false, "Use font shadow"),
        USE_SCALED_FONT                 ("useScaledFont", true, "Use 0.5x scale font size"),
        USE_TEXT_BACKGROUND             ("useTextBackground", true, "Use a solid background color behind the text");

        private final String name;
        private final ConfigType type;
        private String comment;
        private boolean valueBoolean;
        private int valueInteger;
        private String valueString;

        private Generic(String name, boolean defaultValue, String comment)
        {
            this.type = ConfigType.BOOLEAN;
            this.name = name;
            this.valueBoolean = defaultValue;
            this.comment = comment;
        }

        private Generic(String name, int defaultValue, String comment)
        {
            this.type = ConfigType.INTEGER;
            this.name = name;
            this.valueInteger = defaultValue;
            this.comment = comment;
        }

        private Generic(String name, String defaultValue, String comment)
        {
            this.type = ConfigType.STRING;
            this.name = name;
            this.valueString = defaultValue;
            this.comment = comment;
        }

        private Generic(String name, String defaultValue, boolean isColor, String comment)
        {
            this.type = ConfigType.HEX_STRING;
            this.name = name;
            this.valueString = defaultValue;
            this.valueInteger = getColor(defaultValue, 0);
            this.comment = comment;
        }

        @Override
        public ConfigType getType()
        {
            return this.type;
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
            return comment;
        }

        public void setComment(String comment)
        {
            this.comment = comment;
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

        public int getIntegerValue()
        {
            return this.valueInteger;
        }

        public void setIntegerValue(int value)
        {
            this.valueInteger = value;
        }

        public String getStringValue()
        {
            switch (this.type)
            {
                case BOOLEAN:       return String.valueOf(this.valueBoolean);
                case INTEGER:       return String.valueOf(this.valueInteger);
                case HEX_STRING:    return String.format("0x%08X", this.valueInteger);
                case STRING:
                default:            return this.valueString;
            }
        }

        public void setStringValue(String value)
        {
            this.valueString = value;
        }

        public void setColorValue(String str)
        {
            this.valueInteger = getColor(str, 0);
        }

        @Override
        public void setValueFromString(String value)
        {
            try
            {
                switch (this.type)
                {
                    case BOOLEAN:
                        this.valueBoolean = Boolean.getBoolean(value);
                        break;
                    case INTEGER:
                        this.valueInteger = Integer.parseInt(value);
                        break;
                    case STRING:
                        this.valueString = value;
                        break;
                    case HEX_STRING:
                        this.valueInteger = getColor(value, 0);
                        break;
                    default:
                }
            }
            catch (Exception e)
            {
                LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
            }
        }

        public void setValueFromJsonPrimitive(JsonPrimitive value)
        {
            try
            {
                switch (this.type)
                {
                    case BOOLEAN:
                        this.valueBoolean = value.getAsBoolean();
                        break;
                    case INTEGER:
                        this.valueInteger = value.getAsInt();
                        break;
                    case STRING:
                        this.valueString = value.getAsString();
                        break;
                    case HEX_STRING:
                        this.valueInteger = getColor(value.getAsString(), 0);
                        break;
                    default:
                }
            }
            catch (Exception e)
            {
                LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
            }
        }

        public JsonPrimitive getAsJsonPrimitive()
        {
            switch (this.type)
            {
                case BOOLEAN:       return new JsonPrimitive(this.getBooleanValue());
                case INTEGER:       return new JsonPrimitive(this.getIntegerValue());
                case STRING:        return new JsonPrimitive(this.getStringValue());
                case HEX_STRING:    return new JsonPrimitive(String.format("0x%08X", this.getIntegerValue()));
                default:
            }

            return new JsonPrimitive(this.getStringValue());
        }
    }

    // Note: the bitmask affects the default ordering of the info lines!
    public enum InfoToggle implements IConfigBoolean, IConfigHotkey
    {
        BIOME                   ("infoBiome",                   false, 0x00040000, "B", "Show the name of the current biome"),
        BIOME_REG_NAME          ("infoBiomeRegistryName",       false, 0x00080000, "B", "Show the registry name of the current biome"),
        BLOCK_POS               ("infoBlockPosition",           false, 0x00000040, "",  "Show the player's block position"),
        BLOCK_PROPS             ("infoBlockProperties",         false, 0x04000000, "P", "Show the BlockState properties and values"),
        CHUNK_POS               ("infoChunkPosition",           false, 0x00000080, "C", "Show the player's current position in the chunk"),
        CHUNK_SECTIONS          ("infoChunkSections",           false, 0x00002000, "C", "Show the currently rendered number of Chunk sections (the C value from F3)"),
        CHUNK_SECTIONS_FULL     ("infoChunkSectionsLine",       false, 0x00004000, "",  "Show the entire line of the C value from the F3 screen"),
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

    public static void load()
    {
        File configFile = new File(LiteLoader.getCommonConfigFolder(), CONFIG_FILE_NAME);

        if (configFile.exists() && configFile.isFile() && configFile.canRead())
        {
            JsonElement element = JsonUtils.parseJsonFile(configFile);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();
                JsonObject objToggles           = JsonUtils.getNestedObject(root, "InfoTypeToggles", false);
                JsonObject objInfoHotkeys       = JsonUtils.getNestedObject(root, "InfoTypeHotkeys", false);
                JsonObject objInfoLineOrders    = JsonUtils.getNestedObject(root, "InfoLineOrders", false);
                JsonObject objGeneric           = JsonUtils.getNestedObject(root, "Generic", false);
                JsonObject objDebugHotkeys      = JsonUtils.getNestedObject(root, "DebugRendererHotkeys", false);

                if (objGeneric != null)
                {
                    for (Generic gen : Generic.values())
                    {
                        if (objGeneric.has(gen.getName()) && objGeneric.get(gen.getName()).isJsonPrimitive())
                        {
                            gen.setValueFromJsonPrimitive(objGeneric.get(gen.getName()).getAsJsonPrimitive());
                        }
                    }
                }

                for (InfoToggle toggle : InfoToggle.values())
                {
                    if (objToggles != null && JsonUtils.hasBoolean(objToggles, toggle.getName()))
                    {
                        toggle.setBooleanValue(JsonUtils.getBoolean(objToggles, toggle.getName()));
                    }

                    if (objInfoHotkeys != null && JsonUtils.hasString(objInfoHotkeys, toggle.getName()))
                    {
                        toggle.setHotkey(JsonUtils.getString(objInfoHotkeys, toggle.getName()));
                    }

                    if (objInfoLineOrders != null && JsonUtils.hasInteger(objInfoLineOrders, toggle.getName()))
                    {
                        toggle.setLinePosition(JsonUtils.getInteger(objInfoLineOrders, toggle.getName()));
                    }
                }

                if (objDebugHotkeys != null)
                {
                    for (DebugHotkeys dbg : DebugHotkeys.values())
                    {
                        if (JsonUtils.hasString(objDebugHotkeys, dbg.getName()))
                        {
                            dbg.setHotkey(JsonUtils.getString(objDebugHotkeys, dbg.getName()));
                        }
                    }
                }
            }
        }

        requiredKey = getKeyModifier(Generic.REQUIRE_HOLDING_KEY.getStringValue());
        enabledInfoTypes = 0;
        HOTKEY_INFO_MAP.clear();

        for (InfoToggle toggle : InfoToggle.values())
        {
            enabledInfoTypes = toggle.applyBitMask(enabledInfoTypes);
            assignHotkey(HOTKEY_INFO_MAP, toggle);
            LINE_ORDER_MAP.put(toggle.getBitMask(), toggle.getLinePosition());
        }

        HOTKEY_DEBUG_MAP.clear();

        for (DebugHotkeys dbg : DebugHotkeys.values())
        {
            assignHotkey(HOTKEY_DEBUG_MAP, dbg.getHotkey(), dbg.getBitMask());
        }

        RenderEventHandler.getInstance().setEnabledMask(enabledInfoTypes);
    }

    public static void save()
    {
        File dir = LiteLoader.getCommonConfigFolder();

        if (dir.exists() && dir.isDirectory())
        {
            File configFile = new File(dir, CONFIG_FILE_NAME);
            FileWriter writer = null;
            JsonObject root = new JsonObject();
            JsonObject objToggles           = JsonUtils.getNestedObject(root, "InfoTypeToggles", true);
            JsonObject objInfoHotkeys       = JsonUtils.getNestedObject(root, "InfoTypeHotkeys", true);
            JsonObject objInfoLineOrders    = JsonUtils.getNestedObject(root, "InfoLineOrders", true);
            JsonObject objGeneric           = JsonUtils.getNestedObject(root, "Generic", true);
            JsonObject objDebugHotkeys      = JsonUtils.getNestedObject(root, "DebugRendererHotkeys", true);

            for (Generic gen : Generic.values())
            {
                objGeneric.add(gen.getName(), gen.getAsJsonPrimitive());
            }

            for (InfoToggle toggle : InfoToggle.values())
            {
                objToggles.add(toggle.getName(), new JsonPrimitive(toggle.getBooleanValue()));
                objInfoHotkeys.add(toggle.getName(), new JsonPrimitive(toggle.getHotkey()));
                objInfoLineOrders.add(toggle.getName(), new JsonPrimitive(toggle.getLinePosition()));
            }

            for (DebugHotkeys dbg : DebugHotkeys.values())
            {
                objDebugHotkeys.add(dbg.getName(), new JsonPrimitive(dbg.getHotkey()));
            }

            try
            {
                writer = new FileWriter(configFile);
                writer.write(JsonUtils.GSON.toJson(root));
                writer.close();
            }
            catch (IOException e)
            {
                LiteModMiniHud.logger.warn("Failed to write configs to file '{}'", configFile.getAbsolutePath(), e);
            }
            finally
            {
                try
                {
                    if (writer != null)
                    {
                        writer.close();
                    }
                }
                catch (Exception e)
                {
                    LiteModMiniHud.logger.warn("Failed to close config file", e);
                }
            }
        }
    }

    private static int getColor(String colorStr, int defaultColor)
    {
        Pattern pattern = Pattern.compile("0x([a-fA-F0-9]{1,8})");
        Matcher matcher = pattern.matcher(colorStr);

        if (matcher.matches())
        {
            try { return (int) Long.parseLong(matcher.group(1), 16); }
            catch (NumberFormatException e) { return defaultColor; }
        }

        try { return Integer.parseInt(colorStr, 10); }
        catch (NumberFormatException e) { return defaultColor; }
    }

    private static KeyModifier getKeyModifier(String value)
    {
        if (value == null)
        {
            return KeyModifier.NONE;
        }

        if (value.equalsIgnoreCase("shift"))
        {
            return KeyModifier.SHIFT;
        }

        if (value.equalsIgnoreCase("ctrl") || value.equalsIgnoreCase("control"))
        {
            return KeyModifier.CONTROL;
        }

        if (value.equalsIgnoreCase("alt"))
        {
            return KeyModifier.ALT;
        }

        return KeyModifier.NONE;
    }

    private static void assignHotkey(Multimap<Integer, Integer> map, InfoToggle toggle)
    {
        assignHotkey(map, toggle.getHotkey(), toggle.getBitMask());
    }

    private static void assignHotkey(Multimap<Integer, Integer> map, String keyName, int bitmask)
    {
        try
        {
            int keyCode = Integer.parseInt(keyName);

            // Don't interpret the numbers 0..9 as raw keycodes, but instead as the number keys (below)
            if (keyCode > Keyboard.KEY_0)
            {
                map.put(keyCode, bitmask);
                return;
            }
        }
        catch (NumberFormatException e)
        {
        }

        int keyCode = Keyboard.getKeyIndex(keyName.toUpperCase());

        if (keyCode != Keyboard.KEY_NONE)
        {
            map.put(keyCode, bitmask);
        }
    }

    public static int getBitmaskForInfoKey(int keyCode)
    {
        return getBitmaskForKey(HOTKEY_INFO_MAP, keyCode);
    }

    public static int getBitmaskForDebugKey(int keyCode)
    {
        return getBitmaskForKey(HOTKEY_DEBUG_MAP, keyCode);
    }

    private static int getBitmaskForKey(Multimap<Integer, Integer> map, int keyCode)
    {
        Collection<Integer> masks = map.get(keyCode);
        int fullMask = 0;

        if (masks != null)
        {
            for (Integer mask : masks)
            {
                fullMask |= mask;
            }
        }

        return fullMask;
    }

    public static int getLinePositionFor(int infoType)
    {
        Integer value = LINE_ORDER_MAP.get(infoType);
        return value != null ? value.intValue() : -1;
    }
}
