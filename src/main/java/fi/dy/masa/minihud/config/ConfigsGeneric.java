package fi.dy.masa.minihud.config;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.annotation.Nullable;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.interfaces.ConfigType;
import fi.dy.masa.minihud.config.interfaces.IConfigBoolean;
import fi.dy.masa.minihud.config.interfaces.IConfigDouble;
import fi.dy.masa.minihud.config.interfaces.IConfigGeneric;
import fi.dy.masa.minihud.config.interfaces.IConfigOptionList;
import fi.dy.masa.minihud.config.interfaces.IConfigOptionListEntry;
import fi.dy.masa.minihud.event.RenderEventHandler.HudAlignment;

public enum ConfigsGeneric implements IConfigGeneric, IConfigBoolean, IConfigDouble, IConfigOptionList
{
    CHUNK_UNLOAD_BUCKET_FONT_SCALE      ("chunkUnloadBucketOverlayFontScale", 0.15F, "The font scale for the Chunk unload order bucket overlay.\n" +
                                                                                     "Valid range: 0.01 - 1.0"),
    CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS  ("chunkUnloadBucketOverlayChunkRadius", 4, "The radius of chunks to render the text for in the overlay.\n" +
                                                                                   "Valid range: 0 - 10"),
    COORDINATE_FORMAT_STRING        ("coordinateFormat", "x: %.1f y: %.1f z: %.1f",
                                    "The format string for the coordinate line.\n" +
                                    "Needs to have three %f format strings! Default: x: %.1f y: %.1f z: %.1f"),
    DATE_FORMAT_REAL                ("dateFormatReal", "yyyy-MM-dd HH:mm:ss",
                                    "The format string for real time, see the Java SimpleDateFormat\n" +
                                    "class for the format patterns, if needed."),
    DATE_FORMAT_MINECRAFT           ("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx",
                                    "The format string for the Minecraft time.\n" +
                                    "The supported placeholders are: {DAY}, {HOUR}, {MIN}, {SEC}"),
    DEBUG_RENDERER_PATH_MAX_DIST    ("debugRendererPathFindingEnablePointWidth", true,
                                    "If true, then the vanilla pathfinding debug renderer will render the path point width boxes."),
    ENABLE_BY_DEFAULT               ("enableByDefault", true, "If true, the HUD will be enabled by default on game launch"),
    FIX_VANILLA_DEBUG_RENDERERS     ("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed."),
    FONT_COLOR                      ("fontColor", "0xE0E0E0", true, "Font color (RGB, default: 0xE0E0E0 = 14737632)"),
    FONT_SCALE                      ("fontScale", 1.0d, "Font scale factor. Valid range: 0.0 - 10.0. Default: 1.0\n" +
                                                        "Note that the 'useScaledFont' option will override\n" +
                                                        "this option (with 0.5) if it's enabled!"),
    HUD_ALIGNMENT                   ("hudAlignment", HudAlignment.TOP_LEFT, "The alignment of the HUD."),
    REGION_OVERLAY_COLOR            ("regionOverlayColor", "0xFFFF8019", true, "Color for the region file overlay (ARGB, default: 0xFFFF8019)"),
    REQUIRE_SNEAK                   ("requireSneak", false, "Require the player to be sneaking to render the HUD"),
    REQUIRE_HOLDING_KEY             ("requireHoldingKey", "none", "Require holding a key to render the HUD. Valid keys are 'alt', 'ctrl' and 'shift'."),
    SORT_LINES_BY_LENGTH            ("sortLinesByLength", false, "Sort the lines by their text's length"),
    SORT_LINES_REVERSED             ("sortLinesReversed", false, "Reverse the line sorting order"),
    SPAWN_OVERLAY_COLOR_ENTITY      ("spawnOverlayColorEntity", "0xFF30FF20", true, "Color for the entity-processing spawn chunks overlay (ARGB, default: 0xFF30FF20)"),
    SPAWN_OVERLAY_COLOR_LAZY        ("spawnOverlayColorLazy", "0xFFFF3020", true, "Color for the lazy spawn chunks overlay (ARGB, default: 0xFFFF3020)"),
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
    private double valueDouble;
    private String valueString;
    private IConfigOptionListEntry valueOptionList;

    private ConfigsGeneric(String name, boolean defaultValue, String comment)
    {
        this.type = ConfigType.BOOLEAN;
        this.name = name;
        this.valueBoolean = defaultValue;
        this.comment = comment;
    }

    private ConfigsGeneric(String name, int defaultValue, String comment)
    {
        this.type = ConfigType.INTEGER;
        this.name = name;
        this.valueInteger = defaultValue;
        this.comment = comment;
    }

    private ConfigsGeneric(String name, double defaultValue, String comment)
    {
        this.type = ConfigType.DOUBLE;
        this.name = name;
        this.valueDouble = defaultValue;
        this.comment = comment;
    }

    private ConfigsGeneric(String name, String defaultValue, String comment)
    {
        this.type = ConfigType.STRING;
        this.name = name;
        this.valueString = defaultValue;
        this.comment = comment;
    }

    private ConfigsGeneric(String name, String defaultValue, boolean isColor, String comment)
    {
        this.type = ConfigType.HEX_STRING;
        this.name = name;
        this.valueString = defaultValue;
        this.valueInteger = getColor(defaultValue, 0);
        this.comment = comment;
    }

    private ConfigsGeneric(String name, IConfigOptionListEntry option, String comment)
    {
        this.type = ConfigType.OPTION_LIST;
        this.name = name;
        this.valueOptionList = option;
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

    @Override
    public double getDoubleValue()
    {
        return this.valueDouble;
    }

    @Override
    public void setDoubleValue(double value)
    {
        this.valueDouble = value;
    }

    @Override
    public IConfigOptionListEntry getOptionListValue()
    {
        return this.valueOptionList;
    }

    @Override
    public void setOptionListValue(IConfigOptionListEntry value)
    {
        this.valueOptionList = value;
    }

    public String getStringValue()
    {
        switch (this.type)
        {
            case BOOLEAN:       return String.valueOf(this.valueBoolean);
            case INTEGER:       return String.valueOf(this.valueInteger);
            case DOUBLE:        return String.valueOf(this.valueDouble);
            case HEX_STRING:    return String.format("0x%08X", this.valueInteger);
            case OPTION_LIST:   return this.valueOptionList.getStringValue();
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
                case DOUBLE:
                    this.valueDouble = Double.parseDouble(value);
                    break;
                case STRING:
                    this.valueString = value;
                    break;
                case HEX_STRING:
                    this.valueInteger = getColor(value, 0);
                    break;
                case OPTION_LIST:
                    this.valueOptionList = this.valueOptionList.fromString(value);
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
                case DOUBLE:
                    this.valueDouble = value.getAsDouble();
                    break;
                case STRING:
                    this.valueString = value.getAsString();
                    break;
                case HEX_STRING:
                    this.valueInteger = getColor(value.getAsString(), 0);
                    break;
                case OPTION_LIST:
                    this.valueOptionList = this.valueOptionList.fromString(value.getAsString());
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
            case DOUBLE:        return new JsonPrimitive(this.getDoubleValue());
            case STRING:        return new JsonPrimitive(this.getStringValue());
            case HEX_STRING:    return new JsonPrimitive(String.format("0x%08X", this.getIntegerValue()));
            case OPTION_LIST:   return new JsonPrimitive(this.getStringValue());
            default:
        }

        return new JsonPrimitive(this.getStringValue());
    }

    public static int getColor(String colorStr, int defaultColor)
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
}
