package fi.dy.masa.minihud.config;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.lwjgl.input.Keyboard;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.common.config.ConfigCategory;
import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.RenderEventHandler;

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

    public static int defaultMode;
    public static int fontColor;
    public static int textBackgroundColor;
    public static int textPosX;
    public static int textPosY;

    public static String coordinateFormat;
    public static String dateFormatReal;
    public static KeyModifier requiredKey;
    private static final Map<Integer, Integer> HOTKEY_INFO_MAP = new HashMap<Integer, Integer>();

    public static File configurationFile;
    public static Configuration config;
    
    public static final String CATEGORY_GENERIC = "Generic";
    public static final String CATEGORY_INFO_TOGGLE = "InfoTypes";
    public static final String CATEGORY_INFO_HOTKEYS = "InfoToggleHotkeys";

    @SubscribeEvent
    public void onConfigChangedEvent(OnConfigChangedEvent event)
    {
        if (Reference.MOD_ID.equals(event.getModID()) == true)
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
        int defaultModeNumeric = defaultMode;
        boolean defaultModeNumericEnabled = false;
        Property prop;

        prop = conf.get(CATEGORY_GENERIC, "enableByDefault", true);
        prop.setComment("If true, the HUD will be enabled by default on game launch");
        enableByDefault = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "coordinateFormat", "x: %.0f y: %.0f z: %.0f");
        prop.setComment("The format string for the coordinate line (needs to have three %f format strings!) Default: x: %.0f y: %.0f z: %.0f");
        coordinateFormat = prop.getString();

        prop = conf.get(CATEGORY_GENERIC, "coordinateFormatCustomized", true);
        prop.setComment("Use the customized coordinate format string");
        coordinateFormatCustomized = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "dateFormatReal", "HH:mm:ss");
        prop.setComment("The format string for real time, see the Java SimpleDateFormat class for the format patterns, if needed");
        dateFormatReal = prop.getString();

        prop = conf.get(CATEGORY_GENERIC, "defaultMode", 1);
        prop.setComment("Bit mask of the enabled information. 1 = coordinates, 2 = yaw, 4 = pitch, 8 = speed, 16 = biome, 32 = light, 64 = facing, 128 = block, 256 = chunk, 512 = looking at, 1024 = fps, 2048 = entity count, 4096 = dimension id, 8192 = world time (sum together the ones you want enabled by default)");
        defaultModeNumeric = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "defaultModeNumeric", false);
        prop.setComment("Use the numeric bitmask instead of the individual toggle buttons for the info types");
        defaultModeNumericEnabled = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "fontColor", "0xE0E0E0");
        prop.setComment("Font color (RGB, default: 0xE0E0E0 = 14737632)");
        fontColor = getColor(prop.getString(), 0xE0E0E0);

        prop = conf.get(CATEGORY_GENERIC, "sortLinesByLength", false);
        prop.setComment("Sort the lines by their text's length");
        sortLinesByLength = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "requireSneak", false);
        prop.setComment("Require the player to be sneaking to render the HUD");
        requireSneak = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "requireHoldingKey", false);
        prop.setComment("Require holding a key to render the HUD. Valid keys are Alt, Ctrl and Shift.");
        requireHoldingKey = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "requiredKey", "none");
        prop.setComment("The key required to render the HUD, if 'requireHoldingKey' is enabled. Valid values are 'alt', 'ctrl' and 'shift'.");
        requiredKey = getKeyModifier(prop.getString());

        prop = conf.get(CATEGORY_GENERIC, "sortLinesReversed", false);
        prop.setComment("Reverse the line sorting order");
        sortLinesReversed = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "textBackgroundColor", "0x70505050");
        prop.setComment("Text background color (ARGB, default: 0x70505050 = 1884311632)");
        textBackgroundColor = getColor(prop.getString(), 0x70505050);

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
        prop.setComment("Use 0.5x scale font size");
        useScaledFont = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useTextBackground", true);
        prop.setComment("Use a solid background color behind the text");
        useTextBackground = prop.getBoolean();

        // Information types individual toggle

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBiome", false);
        prop.setComment("Show the current biome");
        setInfoType(RenderEventHandler.MASK_BIOME, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBlockPosition", false);
        prop.setComment("Show player's block position");
        setInfoType(RenderEventHandler.MASK_BLOCK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBlockProperties", false);
        prop.setComment("Show the BlockState properties and values");
        setInfoType(RenderEventHandler.MASK_BLOCK_PROPERTIES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoChunkPosition", false);
        prop.setComment("Show player's current position in the chunk");
        setInfoType(RenderEventHandler.MASK_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoCoordinates", true);
        prop.setComment("Show player coordinates");
        setInfoType(RenderEventHandler.MASK_COORDINATES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoDimensionId", true);
        prop.setComment("Show the current dimension ID (might not be accurate in every case, depending on the server!)");
        setInfoType(RenderEventHandler.MASK_DIMENSION, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoEntities", false);
        prop.setComment("Show the visible/loaded entity count");
        setInfoType(RenderEventHandler.MASK_ENTITIES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoFacing", true);
        prop.setComment("Show player facing");
        setInfoType(RenderEventHandler.MASK_FACING, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoFPS", false);
        prop.setComment("Show current FPS");
        setInfoType(RenderEventHandler.MASK_FPS, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLightLevel", false);
        prop.setComment("Show the current light level");
        setInfoType(RenderEventHandler.MASK_LIGHT, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLookingAt", false);
        prop.setComment("Show which block the player is looking at");
        setInfoType(RenderEventHandler.MASK_LOOKINGAT, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLookingAtEntity", false);
        prop.setComment("Show entity name and health when looked at");
        setInfoType(RenderEventHandler.MASK_LOOKING_AT_ENTITY, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRealTime", false);
        prop.setComment("Show the current real time formatted according to dateFormatReal");
        setInfoType(RenderEventHandler.MASK_TIME_REAL, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRotationPitch", false);
        prop.setComment("Show player pitch rotation");
        setInfoType(RenderEventHandler.MASK_PITCH, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRotationYaw", false);
        prop.setComment("Show player yaw rotation");
        setInfoType(RenderEventHandler.MASK_YAW, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoSlimeChunk", false);
        prop.setComment("Show whether the player is currently in a slime chunk.\n" +
                        "NOTE: This only works in single player without any user intervention!\n" +
                        "On a server the player needs to be admin/OP and\n" +
                        "run the /seed command manually EVERY TIME they join or change dimensions!");
        setInfoType(RenderEventHandler.MASK_SLIME_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoSpeed", false);
        prop.setComment("Show player moving speed");
        setInfoType(RenderEventHandler.MASK_SPEED, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoWorldTime", false);
        prop.setComment("Show the current world time in ticks");
        setInfoType(RenderEventHandler.MASK_TIME_TICKS, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoWorldTimeFormatted", true);
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

        assignInfoHotkey(conf, "infoCoordinates",           RenderEventHandler.MASK_COORDINATES         , "1");
        assignInfoHotkey(conf, "infoRotationYaw",           RenderEventHandler.MASK_YAW                 , "2");
        assignInfoHotkey(conf, "infoRotationPitch",         RenderEventHandler.MASK_PITCH               , "3");
        assignInfoHotkey(conf, "infoSpeed",                 RenderEventHandler.MASK_SPEED               , "4");
        assignInfoHotkey(conf, "infoBiome",                 RenderEventHandler.MASK_BIOME               , "5");
        assignInfoHotkey(conf, "infoLightLevel",            RenderEventHandler.MASK_LIGHT               , "6");
        assignInfoHotkey(conf, "infoFacing",                RenderEventHandler.MASK_FACING              , "7");
        assignInfoHotkey(conf, "infoBlockPosition",         RenderEventHandler.MASK_BLOCK               , "8");
        assignInfoHotkey(conf, "infoChunkPosition",         RenderEventHandler.MASK_CHUNK               , "9");
        assignInfoHotkey(conf, "infoLookingAt",             RenderEventHandler.MASK_LOOKINGAT           , "0");
        assignInfoHotkey(conf, "infoFPS",                   RenderEventHandler.MASK_FPS                 , "a");
        assignInfoHotkey(conf, "infoEntities",              RenderEventHandler.MASK_ENTITIES            , "b");
        assignInfoHotkey(conf, "infoDimensionId",           RenderEventHandler.MASK_DIMENSION           , "c");
        assignInfoHotkey(conf, "infoWorldTime",             RenderEventHandler.MASK_TIME_TICKS          , "d");
        assignInfoHotkey(conf, "infoWorldTimeFormatted",    RenderEventHandler.MASK_TIME_MC             , "e");
        assignInfoHotkey(conf, "infoRealTime",              RenderEventHandler.MASK_TIME_REAL           , "f");
        assignInfoHotkey(conf, "infoLookingAtEntity",       RenderEventHandler.MASK_LOOKING_AT_ENTITY   , "g");
        assignInfoHotkey(conf, "infoSlimeChunk",            RenderEventHandler.MASK_SLIME_CHUNK         , "i");
        assignInfoHotkey(conf, "infoBlockProperties",       RenderEventHandler.MASK_BLOCK_PROPERTIES    , "j");

        if (defaultModeNumericEnabled)
        {
            defaultMode = defaultModeNumeric;
        }

        RenderEventHandler.getInstance().setEnabledMask(defaultMode);

        if (conf.hasChanged())
        {
            conf.save();
        }
    }

    private static void setInfoType(int mask, boolean value)
    {
        if (value)
        {
            defaultMode |= mask;
        }
        else
        {
            defaultMode &= ~mask;
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

    private static void assignInfoHotkey(Configuration conf, String configKey, int infoBitmask, String defaultValue)
    {
        String keyName = conf.get(CATEGORY_INFO_HOTKEYS, configKey, defaultValue).getString();

        try
        {
            int keyCode = Integer.parseInt(keyName);

            // Don't interpret the numbers 0..9 as raw keycodes, but instead as the number keys (below)
            if (keyCode > Keyboard.KEY_0)
            {
                HOTKEY_INFO_MAP.put(keyCode, infoBitmask);
                return;
            }
        }
        catch (NumberFormatException e)
        {
        }

        int keyCode = Keyboard.getKeyIndex(keyName.toUpperCase());

        if (keyCode != Keyboard.KEY_NONE)
        {
            HOTKEY_INFO_MAP.put(keyCode, infoBitmask);
        }
    }

    public static int getBitmaskForKey(int keyCode)
    {
        Integer mask = HOTKEY_INFO_MAP.get(keyCode);
        return mask != null ? mask.intValue() : 0;
    }
}
