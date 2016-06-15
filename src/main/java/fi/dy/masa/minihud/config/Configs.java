package fi.dy.masa.minihud.config;

import java.io.File;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.minecraftforge.common.config.Configuration;
import net.minecraftforge.common.config.Property;
import net.minecraftforge.fml.client.event.ConfigChangedEvent.OnConfigChangedEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.RenderEventHandler;

public class Configs
{
    public static boolean sortLinesByLength;
    public static boolean sortLinesReversed;
    public static boolean coordinateFormatCustomized;
    public static boolean useFontShadow;
    public static boolean useScaledFont;
    public static boolean useTextBackground;

    public static int defaultMode;
    public static int fontColor;
    public static int textBackgroundColor;
    public static int textPosX;
    public static int textPosY;

    public static String coordinateFormat;

    public static File configurationFile;
    public static Configuration config;
    
    public static final String CATEGORY_GENERIC = "Generic";
    public static final String CATEGORY_INFO_TOGGLE = "InfoTypes";

    @SubscribeEvent
    public void onConfigChangedEvent(OnConfigChangedEvent event)
    {
        if (Reference.MOD_ID.equals(event.modID) == true)
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
        boolean defaultMoDeNumericEnabled = false;
        Property prop;

        prop = conf.get(CATEGORY_GENERIC, "coordinateFormat", "XYZ: %.4f / %.4f / %.4f");
        prop.comment = "The format string for the coordinate line (needs to have three %f format strings!) Default: XYZ: %.4f / %.4f / %.4f";
        coordinateFormat = prop.getString();

        prop = conf.get(CATEGORY_GENERIC, "coordinateFormatCustomized", false);
        prop.comment = "Use the customized coordinate format string";
        coordinateFormatCustomized = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "defaultMode", 1);
        prop.comment = "Bit mask of the enabled information. 1 = coordinates, 2 = yaw, 4 = pitch, 8 = speed, 16 = biome, 32 = light, 64 = facing, 128 = block, 256 = chunk, 512 = looking at, 1024 = fps, 2048 = entity count (sum together the ones you want enabled by default)";
        defaultModeNumeric = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "defaultModeNumeric", false);
        prop.comment = "Use the numeric bitmask instead of the individual toggle buttons for the info types";
        defaultMoDeNumericEnabled = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "fontColor", "0xE0E0E0");
        prop.comment = "Font color (default: 0xE0E0E0 = 14737632)";
        fontColor = getColor(prop.getString(), 0xE0E0E0);

        prop = conf.get(CATEGORY_GENERIC, "sortLinesByLength", false);
        prop.comment = "Sort the lines by their text's length";
        sortLinesByLength = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "sortLinesReversed", false);
        prop.comment = "Reverse the line sorting order";
        sortLinesReversed = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "textBackgroundColor", "0x70505050");
        prop.comment = "Text background color (default: 0x70505050 = 1884311632)";
        textBackgroundColor = getColor(prop.getString(), 0x70505050);

        prop = conf.get(CATEGORY_GENERIC, "textPosX", 4);
        prop.comment = "Text X position (default: 4)";
        textPosX = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "textPosY", 4);
        prop.comment = "Text Y position (default: 4)";
        textPosY = prop.getInt();

        prop = conf.get(CATEGORY_GENERIC, "useFontShadow", false);
        prop.comment = "Use font shadow";
        useFontShadow = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useScaledFont", true);
        prop.comment = "Use 0.5x scale font size";
        useScaledFont = prop.getBoolean();

        prop = conf.get(CATEGORY_GENERIC, "useTextBackground", true);
        prop.comment = "Use a solid background color behind the text";
        useTextBackground = prop.getBoolean();

        // Information types individual toggle

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoCoordinates", false);
        prop.comment = "Show player coordinates";
        setInfoType(RenderEventHandler.MASK_COORDINATES, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRotationYaw", false);
        prop.comment = "Show player yaw rotation";
        setInfoType(RenderEventHandler.MASK_YAW, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoRotationPitch", false);
        prop.comment = "Show player pitch rotation";
        setInfoType(RenderEventHandler.MASK_PITCH, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoSpeed", false);
        prop.comment = "Show player moving speed";
        setInfoType(RenderEventHandler.MASK_SPEED, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBiome", false);
        prop.comment = "Show the current biome";
        setInfoType(RenderEventHandler.MASK_BIOME, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLightLevel", false);
        prop.comment = "Show the current light level";
        setInfoType(RenderEventHandler.MASK_LIGHT, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoFacing", false);
        prop.comment = "Show player facing";
        setInfoType(RenderEventHandler.MASK_FACING, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoBlockPosition", false);
        prop.comment = "Show player's block position";
        setInfoType(RenderEventHandler.MASK_BLOCK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoChunkPosition", false);
        prop.comment = "Show player's current position in the chunk";
        setInfoType(RenderEventHandler.MASK_CHUNK, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoLookingAt", false);
        prop.comment = "Show which block the player is looking at";
        setInfoType(RenderEventHandler.MASK_LOOKINGAT, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoFPS", false);
        prop.comment = "Show current FPS";
        setInfoType(RenderEventHandler.MASK_FPS, prop.getBoolean());

        prop = conf.get(CATEGORY_INFO_TOGGLE, "infoEntities", false);
        prop.comment = "Show the entity count";
        setInfoType(RenderEventHandler.MASK_ENTITIES, prop.getBoolean());

        if (defaultMoDeNumericEnabled == true)
        {
            defaultMode = defaultModeNumeric;
        }

        RenderEventHandler.getInstance().setEnabledMask(defaultMode);

        if (conf.hasChanged() == true)
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
        Pattern pattern = Pattern.compile("0x([0-9A-F]{1,8})");
        Matcher matcher = pattern.matcher(colorStr);

        if (matcher.matches())
        {
            try { return Integer.parseInt(matcher.group(1), 16); }
            catch (NumberFormatException e) { return defaultColor; }
        }

        try { return Integer.parseInt(colorStr, 10); }
        catch (NumberFormatException e) { return defaultColor; }
    }
}
