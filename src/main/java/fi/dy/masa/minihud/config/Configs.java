package fi.dy.masa.minihud.config;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import net.minecraft.util.math.MathHelper;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.InputEventHandler;
import fi.dy.masa.minihud.event.RenderEventHandler;
import fi.dy.masa.minihud.event.RenderEventHandler.HudAlignment;

public class Configs
{
    public static boolean enableByDefault;
    public static boolean sortLinesByLength;
    public static boolean sortLinesReversed;
    public static boolean coordinateFormatCustomized;
    public static boolean requireSneak;
    public static boolean requireHoldingKey;
    public static boolean useFontShadow;
    public static boolean useScaledFont;
    public static boolean useTextBackground;

    public static boolean debugRendererPathfindingEnablePointWidth;

    private static double fontScale;
    public static double activeFontScale;

    public static int enabledInfoTypes;
    public static int fontColor;
    public static int regionOverlayColor;
    public static int textBackgroundColor;
    public static int textPosX;
    public static int textPosY;

    public static String coordinateFormat;
    public static String dateFormatMinecraft;
    public static String dateFormatReal;

    public static HudAlignment hudAlignment = HudAlignment.TOP_LEFT;

    public static KeyModifier requiredKey;
    private static final Multimap<Integer, Long> HOTKEY_DEBUG_MAP = HashMultimap.create();
    private static final Multimap<Integer, Long> HOTKEY_INFO_MAP = HashMultimap.create();
    private static final Map<Long, Integer> LINE_ORDER_MAP = new HashMap<>();

    public static File configurationFile;
    public static Configuration config;
    
    public static final String CATEGORY_DEBUG_HOTKEYS = "VanillaDebugRendererHotkeys";
    public static final String CATEGORY_DEBUG_RENDERER = "VanillaDebugRendererOptions";
    public static final String CATEGORY_GENERIC = "Generic";
    public static final String CATEGORY_INFO_TOGGLE = "InfoTypes";
    public static final String CATEGORY_INFO_HOTKEYS = "InfoToggleHotkeys";
    public static final String CATEGORY_INFO_LINE_ORDER = "InfoLineOrder";
    public static final String CATEGORY_RENDERER_HOTKEYS = "RenderHotkeys";

    @SubscribeEvent
    public void onConfigChangedEvent(OnConfigChangedEvent event)
    {
        if (Reference.MOD_ID.equals(event.getModID()))
        {
            loadConfigs(config);
        }
    }

    public static void loadConfigsFromFile(File configFile)
    {
        configurationFile = configFile;
        config = new Configuration(configFile, null, true);
        config.load();

        loadConfigs(config);
    }

    public static void loadConfigs(Configuration conf)
    {
        Property prop;

        prop = conf.get(CATEGORY_GENERIC, "coordinateFormat", "x: %.1f y: %.1f z: %.1f");
        prop.setComment("The format string for the coordinate line (needs to have three %f format strings!) Default: x: %.1f y: %.1f z: %.1f");
        coordinateFormat = prop.getString();

        prop = conf.get(CATEGORY_GENERIC, "coordinateFormatCustomized", true);
        prop.setComment("Use the customized coordinate format string");
        coordinateFormatCustomized = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "dateFormatReal", "yyyy-MM-dd HH:mm:ss");
        prop.setComment("The format string for real time, see the Java SimpleDateFormat class for the format patterns, if needed");
        dateFormatReal = prop.getString();

        prop = conf.get(CATEGORY_GENERIC, "dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx");
        prop.setComment("The format string for the Minecraft time.\n" +
                        "The supported placeholders are: {DAY}, {HOUR}, {MIN}, {SEC}");
        dateFormatMinecraft = prop.getString();

        prop = conf.get(CATEGORY_GENERIC, "enableByDefault", true);
        prop.setComment("If true, the HUD will be enabled by default on game launch");
        enableByDefault = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "fontColor", "0xE0E0E0");
        prop.setComment("Font color (RGB, default: 0xE0E0E0 = 14737632)");
        fontColor = getColor(prop.getString(), 0xE0E0E0);

        prop = conf.get(CATEGORY_GENERIC, "fontScale", 1.0d);
        prop.setComment("Font scale factor. Valid range: 0.0 - 10.0. Default: 1.0\n" +
                        "Note that the 'useScaledFont' option will override this option (with 0.5) if it's enabled!");
        fontScale = MathHelper.clamp(prop.getDouble(), 0d, 10d);

        prop = conf.get(CATEGORY_GENERIC, "hudAlignment", "top_left");
        prop.setComment("The alignment of the HUD. Valid values: top_left, rop_right, bottom_left, bottom_right, center.");
        hudAlignment = HudAlignment.fromString(prop.getString());

        prop = conf.get(CATEGORY_GENERIC, "regionOverlayColor", "0xFFFF8019");
        prop.setComment("Color for the region file overlay rendering (RGB, default: 0xFFFF8019)");
        regionOverlayColor = getColor(prop.getString(), 0xFFFF8019);

        prop = conf.get(CATEGORY_GENERIC, "requireSneak", false);
        prop.setComment("Require the player to be sneaking to render the HUD");
        requireSneak = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "requireHoldingKey", false);
        prop.setComment("Require holding a key to render the HUD. Valid keys are Alt, Ctrl and Shift.");
        requireHoldingKey = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "requiredKey", "none");
        prop.setComment("The key required to render the HUD, if 'requireHoldingKey' is enabled. Valid values are 'alt', 'ctrl' and 'shift'.");
        requiredKey = getKeyModifier(prop.getString());

        prop = conf.get(CATEGORY_GENERIC, "sortLinesByLength", false);
        prop.setComment("Sort the lines by their text's length");
        sortLinesByLength = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "sortLinesReversed", false);
        prop.setComment("Reverse the line sorting order");
        sortLinesReversed = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "textBackgroundColor", "0xA0505050");
        prop.setComment("Text background color (ARGB, default: 0xA0505050)");
        textBackgroundColor = getColor(prop.getString(), 0xA0505050);

        prop = conf.get(CATEGORY_GENERIC, "textPosX", 4);
        prop.setComment("Text X position (default: 4)");
        textPosX = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "textPosY", 4);
        prop.setComment("Text Y position (default: 4)");
        textPosY = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "useFontShadow", false);
        prop.setComment("Use font shadow");
        useFontShadow = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useScaledFont", true);
        prop.setComment("Use 0.5x scale font size. Note that this overrides the 'fontScale' option!");
        useScaledFont = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useTextBackground", true);
        prop.setComment("Use a solid background color behind the text");
        useTextBackground = prop.getBoolean();


        // Debug renderer related options

        prop = conf.get(CATEGORY_DEBUG_RENDERER, "debugRendererPathfindingEnablePointWidth", true);
        prop.setComment("If true, then the vanilla pathfinding debug renderer will render the max distance boxes.");
        debugRendererPathfindingEnablePointWidth = prop.getBoolean();


        // Information types individual toggle

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBiome", false);
        prop.setComment("Show the the name of the current biome");
        setInfoType(RenderEventHandler.MASK_BIOME, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBiomeRegistryName", false);
        prop.setComment("Show the registry name of the current biome");
        setInfoType(RenderEventHandler.MASK_BIOME_REGISTRY_NAME, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBlockPosition", false);
        prop.setComment("Show the player's current block position");
        setInfoType(RenderEventHandler.MASK_BLOCK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBlockProperties", false);
        prop.setComment("Show the BlockState properties and values of the looked-at block");
        setInfoType(RenderEventHandler.MASK_BLOCK_PROPERTIES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBlockInChunk", false);
        prop.setComment("Show the player's current position within the chunk");
        setInfoType(RenderEventHandler.MASK_BLOCK_IN_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoChunkPosition", false);
        prop.setComment("Show the chunk position the player is currently in");
        setInfoType(RenderEventHandler.MASK_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoChunkSections", false);
        prop.setComment("Show the currently rendered number of Chunk sections (the C value from F3)");
        setInfoType(RenderEventHandler.MASK_CHUNK_SECTIONS, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoChunkSectionsLine", false);
        prop.setComment("Show the entire line of the C value from the F3 screen");
        setInfoType(RenderEventHandler.MASK_CHUNK_SECTIONS_LINE, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoChunkUpdates", false);
        prop.setComment("Show the current number of chunk updates per second");
        setInfoType(RenderEventHandler.MASK_CHUNK_UPDATES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoCoordinates", true);
        prop.setComment("Show the player's current coordinates");
        setInfoType(RenderEventHandler.MASK_COORDINATES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoDifficulty", false);
        prop.setComment("Show the local difficulty");
        setInfoType(RenderEventHandler.MASK_DIFFICULTY, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoDimensionId", true);
        prop.setComment("Show the current dimension ID (might not be accurate in every case, depending on the server!)");
        setInfoType(RenderEventHandler.MASK_DIMENSION, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoEntities", false);
        prop.setComment("Show the visible/loaded entity count");
        setInfoType(RenderEventHandler.MASK_ENTITIES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoEntityRegistryName", false);
        prop.setComment("Show the registry name of the entity the player is currently looking at");
        setInfoType(RenderEventHandler.MASK_LOOKING_AT_ENTITY_REGNAME, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoFacing", true);
        prop.setComment("Show the player's current facing");
        setInfoType(RenderEventHandler.MASK_FACING, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoFPS", false);
        prop.setComment("Show the current FPS");
        setInfoType(RenderEventHandler.MASK_FPS, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLightLevel", false);
        prop.setComment("Show the current light level");
        setInfoType(RenderEventHandler.MASK_LIGHT, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLookingAtBlock", false);
        prop.setComment("Show which block the player is currently looking at");
        setInfoType(RenderEventHandler.MASK_LOOKING_AT_BLOCK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLookingAtBlockInChunk", false);
        prop.setComment("Show which block within its containing chunk the player is currently looking at");
        setInfoType(RenderEventHandler.MASK_LOOKING_AT_BLOCK_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLookingAtEntity", false);
        prop.setComment("Show the entity name and health when looked at");
        setInfoType(RenderEventHandler.MASK_LOOKING_AT_ENTITY, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoParticleCount", false);
        prop.setComment("Show the currently renderer particle count (P from F3)");
        setInfoType(RenderEventHandler.MASK_PARTICLE_COUNT, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRealTime", true);
        prop.setComment("Show the current real time formatted according to dateFormatReal");
        setInfoType(RenderEventHandler.MASK_TIME_REAL, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRegionFile", false);
        prop.setComment("Show the region file the player is currently in");
        setInfoType(RenderEventHandler.MASK_REGION_FILE, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRotationPitch", false);
        prop.setComment("Show the player's current pitch rotation");
        setInfoType(RenderEventHandler.MASK_PITCH, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRotationYaw", false);
        prop.setComment("Show the player's current yaw rotation");
        setInfoType(RenderEventHandler.MASK_YAW, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoSlimeChunk", false);
        prop.setComment("Show whether the player is currently in a slime chunk.\n" +
                        "NOTE: This only works in single player without any user intervention!\n" +
                        "On a server the player needs to be admin/OP and\n" +
                        "run the /seed command manually EVERY TIME they join or change dimensions!");
        setInfoType(RenderEventHandler.MASK_SLIME_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoSpeed", false);
        prop.setComment("Show the player's current moving speed");
        setInfoType(RenderEventHandler.MASK_SPEED, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoWorldTime", false);
        prop.setComment("Show the current world time in ticks");
        setInfoType(RenderEventHandler.MASK_TIME_TICKS, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoWorldTimeFormatted", false);
        prop.setComment("Show the current world time formatted to days, hours, minutes");
        setInfoType(RenderEventHandler.MASK_TIME_MC, prop.getBoolean());

        // Info hotkey assignments
        ConfigCategory cat = conf.getCategory(CATEGORY_INFO_HOTKEYS);
        cat.setComment("Here you can assign hotkeys to toggle the different information types on/off.\n" +
                        "NOTE: Make sure you don't have the same value for more than one info type, or " +
                        "the latest one will override all the earlier ones.\n" +
                        "You can give the key \"names\", like \"a\" or \"7\", or you can give " +
                        "the raw numeric LWJGL keycodes (only works for key codes > 11).\n" +
                        "To toggle the info type while in-game, press and hold the Toggle key and then " +
                        "press the keys set here to toggle the info types on/off.");

        HOTKEY_INFO_MAP.clear();

        assignInfoHotkey(conf, "infoFPS",                   RenderEventHandler.MASK_FPS                         , "");
        assignInfoHotkey(conf, "infoRealTime",              RenderEventHandler.MASK_TIME_REAL                   , "t");
        assignInfoHotkey(conf, "infoWorldTime",             RenderEventHandler.MASK_TIME_TICKS                  , "");
        assignInfoHotkey(conf, "infoWorldTimeFormatted",    RenderEventHandler.MASK_TIME_MC                     , "");
        assignInfoHotkey(conf, "infoCoordinates",           RenderEventHandler.MASK_COORDINATES                 , "n");
        assignInfoHotkey(conf, "infoDimensionId",           RenderEventHandler.MASK_DIMENSION                   , "d");
        assignInfoHotkey(conf, "infoBlockPosition",         RenderEventHandler.MASK_BLOCK                       , "o");
        assignInfoHotkey(conf, "infoBlockInChunk",          RenderEventHandler.MASK_BLOCK_IN_CHUNK              , "");
        assignInfoHotkey(conf, "infoFacing",                RenderEventHandler.MASK_FACING                      , "f");
        assignInfoHotkey(conf, "infoLightLevel",            RenderEventHandler.MASK_LIGHT                       , "l");
        assignInfoHotkey(conf, "infoRotationYaw",           RenderEventHandler.MASK_YAW                         , "r");
        assignInfoHotkey(conf, "infoRotationPitch",         RenderEventHandler.MASK_PITCH                       , "r");
        assignInfoHotkey(conf, "infoSpeed",                 RenderEventHandler.MASK_SPEED                       , "s");
        assignInfoHotkey(conf, "infoChunkSections",         RenderEventHandler.MASK_CHUNK_SECTIONS              , "");
        assignInfoHotkey(conf, "infoChunkSectionsLine",     RenderEventHandler.MASK_CHUNK_SECTIONS_LINE         , "");
        assignInfoHotkey(conf, "infoChunkUpdates",          RenderEventHandler.MASK_CHUNK_UPDATES               , "");
        assignInfoHotkey(conf, "infoParticleCount",         RenderEventHandler.MASK_PARTICLE_COUNT              , "");
        assignInfoHotkey(conf, "infoDifficulty",            RenderEventHandler.MASK_DIFFICULTY                  , "");
        assignInfoHotkey(conf, "infoBiome",                 RenderEventHandler.MASK_BIOME                       , "b");
        assignInfoHotkey(conf, "infoBiomeRegistryName",     RenderEventHandler.MASK_BIOME_REGISTRY_NAME         , "b");
        assignInfoHotkey(conf, "infoEntities",              RenderEventHandler.MASK_ENTITIES                    , "");
        assignInfoHotkey(conf, "infoSlimeChunk",            RenderEventHandler.MASK_SLIME_CHUNK                 , "i");
        assignInfoHotkey(conf, "infoLookingAtEntity",       RenderEventHandler.MASK_LOOKING_AT_ENTITY           , "l");
        assignInfoHotkey(conf, "infoEntityRegistryName",    RenderEventHandler.MASK_LOOKING_AT_ENTITY_REGNAME   , "l");
        assignInfoHotkey(conf, "infoLookingAtBlock",        RenderEventHandler.MASK_LOOKING_AT_BLOCK            , "l");
        assignInfoHotkey(conf, "infoLookingAtBlockInChunk", RenderEventHandler.MASK_LOOKING_AT_BLOCK_CHUNK      , "");
        assignInfoHotkey(conf, "infoBlockProperties",       RenderEventHandler.MASK_BLOCK_PROPERTIES            , "p");
        assignInfoHotkey(conf, "infoChunkPosition",         RenderEventHandler.MASK_CHUNK                       , "c");
        assignInfoHotkey(conf, "infoRegionFile",            RenderEventHandler.MASK_REGION_FILE                 , "g");

        assignHotkey(HOTKEY_INFO_MAP, conf, CATEGORY_RENDERER_HOTKEYS, "renderRegionOverlay", "j", RenderEventHandler.MASK_REGION_OVERLAY);


        cat = conf.getCategory(CATEGORY_DEBUG_HOTKEYS);
        cat.setComment("Hotkeys to toggle ON/OFF the vanilla debug renderers.\n" +
                       "To use these, first press down F3 and then press the\n" +
                       "keys defined here to toggle the feature ON/OFF.");

        HOTKEY_DEBUG_MAP.clear();

        assignDebugRendererHotkey(conf, "debugCollisionBoxEnabled",     InputEventHandler.MASK_DEBUG_COLLISION_BOXES,   "1");
        assignDebugRendererHotkey(conf, "debugHeightMapEnabled",        InputEventHandler.MASK_DEBUG_HEIGHT_MAP,        "2");
        assignDebugRendererHotkey(conf, "debugNeighborsUpdateEnabled",  InputEventHandler.MASK_DEBUG_NEIGHBOR_UPDATE,   "3");
        assignDebugRendererHotkey(conf, "debugPathfindingEnabled",      InputEventHandler.MASK_DEBUG_PATHFINDING,       "4");
        assignDebugRendererHotkey(conf, "debugSolidFaceEnabled",        InputEventHandler.MASK_DEBUG_SOLID_FACES,       "5");
        assignDebugRendererHotkey(conf, "debugWaterEnabled",            InputEventHandler.MASK_DEBUG_WATER,             "6");


        cat = conf.getCategory(CATEGORY_INFO_LINE_ORDER);
        cat.setComment("Here you can set the order of the info lines.\n" +
                       "The number is the position in the list the line gets added to.\n" +
                       "The indexing starts from 0.\n" +
                       "If multiple types have the same index, then the last one that\n" +
                       "is actually added internally, will bump previous entries downwards.");

        LINE_ORDER_MAP.clear();

        setLinePosition(conf, "infoFPS",                    RenderEventHandler.MASK_FPS);
        setLinePosition(conf, "infoRealTime",               RenderEventHandler.MASK_TIME_REAL);
        setLinePosition(conf, "infoWorldTime",              RenderEventHandler.MASK_TIME_TICKS);
        setLinePosition(conf, "infoWorldTimeFormatted",     RenderEventHandler.MASK_TIME_MC);
        setLinePosition(conf, "infoCoordinates",            RenderEventHandler.MASK_COORDINATES);
        setLinePosition(conf, "infoDimensionId",            RenderEventHandler.MASK_DIMENSION);
        setLinePosition(conf, "infoBlockPosition",          RenderEventHandler.MASK_BLOCK);
        setLinePosition(conf, "infoBlockInChunk",           RenderEventHandler.MASK_BLOCK_IN_CHUNK);
        setLinePosition(conf, "infoChunkPosition",          RenderEventHandler.MASK_CHUNK);
        setLinePosition(conf, "infoRegionFile",             RenderEventHandler.MASK_REGION_FILE);
        setLinePosition(conf, "infoFacing",                 RenderEventHandler.MASK_FACING);
        setLinePosition(conf, "infoLightLevel",             RenderEventHandler.MASK_LIGHT);
        setLinePosition(conf, "infoRotationYaw",            RenderEventHandler.MASK_YAW);
        setLinePosition(conf, "infoRotationPitch",          RenderEventHandler.MASK_PITCH);
        setLinePosition(conf, "infoSpeed",                  RenderEventHandler.MASK_SPEED);
        setLinePosition(conf, "infoChunkSections",          RenderEventHandler.MASK_CHUNK_SECTIONS);
        setLinePosition(conf, "infoChunkSectionsLine",      RenderEventHandler.MASK_CHUNK_SECTIONS_LINE);
        setLinePosition(conf, "infoChunkUpdates",           RenderEventHandler.MASK_CHUNK_UPDATES);
        setLinePosition(conf, "infoParticleCount",          RenderEventHandler.MASK_PARTICLE_COUNT);
        setLinePosition(conf, "infoDifficulty",             RenderEventHandler.MASK_DIFFICULTY);
        setLinePosition(conf, "infoBiome",                  RenderEventHandler.MASK_BIOME);
        setLinePosition(conf, "infoBiomeRegistryName",      RenderEventHandler.MASK_BIOME_REGISTRY_NAME);
        setLinePosition(conf, "infoEntities",               RenderEventHandler.MASK_ENTITIES);
        setLinePosition(conf, "infoSlimeChunk",             RenderEventHandler.MASK_SLIME_CHUNK);
        setLinePosition(conf, "infoLookingAtEntity",        RenderEventHandler.MASK_LOOKING_AT_ENTITY);
        setLinePosition(conf, "infoEntityRegistryName",     RenderEventHandler.MASK_LOOKING_AT_ENTITY_REGNAME);
        setLinePosition(conf, "infoLookingAtBlock",         RenderEventHandler.MASK_LOOKING_AT_BLOCK);
        setLinePosition(conf, "infoLookingAtBlockInChunk",  RenderEventHandler.MASK_LOOKING_AT_BLOCK_CHUNK);
        setLinePosition(conf, "infoBlockProperties",        RenderEventHandler.MASK_BLOCK_PROPERTIES);

        RenderEventHandler.getInstance().setEnabledMask(enabledInfoTypes);

        activeFontScale = useScaledFont ? 0.5d : fontScale;

        if (conf.hasChanged())
        {
            conf.save();
        }
    }

    private static void setInfoType(int mask, boolean value)
    {
        if (value)
        {
            enabledInfoTypes |= mask;
        }
        else
        {
            enabledInfoTypes &= ~mask;
        }
    }

    private static int getColor(String colorStr, int defaultColor)
    {
        Pattern pattern = Pattern.compile("(?:0x|#)([a-fA-F0-9]{1,8})");
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

    private static void assignInfoHotkey(Configuration conf, String configKey, long infoBitmask, String defaultKey)
    {
        assignHotkey(HOTKEY_INFO_MAP, conf, CATEGORY_INFO_HOTKEYS, configKey, defaultKey, infoBitmask);
    }

    private static void assignDebugRendererHotkey(Configuration conf, String configKey, int bitmask, String defaultKey)
    {
        assignHotkey(HOTKEY_DEBUG_MAP, conf, CATEGORY_DEBUG_HOTKEYS, configKey, defaultKey, bitmask);
    }

    private static void assignHotkey(Multimap<Integer, Long> map, Configuration conf, String configCategory, String configKey, String defaultKey, long bitmask)
    {
        String keyName = conf.get(configCategory, configKey, defaultKey).getString();

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

    public static long getBitmaskForInfoKey(int keyCode)
    {
        return getBitmaskForKey(HOTKEY_INFO_MAP, keyCode);
    }

    public static long getBitmaskForDebugKey(int keyCode)
    {
        return getBitmaskForKey(HOTKEY_DEBUG_MAP, keyCode);
    }

    public static long getBitmaskForKey(Multimap<Integer, Long> map, int keyCode)
    {
        Collection<Long> masks = map.get(keyCode);
        long fullMask = 0;

        if (masks != null)
        {
            for (Long mask : masks)
            {
                fullMask |= mask;
            }
        }

        return fullMask;
    }

    private static void setLinePosition(Configuration conf, String configKey, long infoBitmask)
    {
        int value = conf.get(CATEGORY_INFO_LINE_ORDER, configKey, -1).getInt();
        LINE_ORDER_MAP.put(infoBitmask, value);
    }

    public static int getLinePositionFor(long infoType)
    {
        Integer value = LINE_ORDER_MAP.get(infoType);
        return value != null ? value.intValue() : -1;
    }
}
