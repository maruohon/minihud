package fi.dy.masa.minihud.config;

import java.io.File;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mumfrey.liteloader.core.LiteLoader;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.HudAlignment;
import fi.dy.masa.malilib.config.IConfigValue;
import fi.dy.masa.malilib.config.options.ConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigColor;
import fi.dy.masa.malilib.config.options.ConfigDouble;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.config.options.ConfigInteger;
import fi.dy.masa.malilib.config.options.ConfigOptionList;
import fi.dy.masa.malilib.config.options.ConfigString;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.RenderEventHandler;

public class Configs
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";

    public static class Generic
    {
        public static final ConfigHotkey        TOGGLE_KEY                          = new ConfigHotkey("toggleKey", "H", "The main toggle key");
        public static final ConfigDouble        CHUNK_UNLOAD_BUCKET_FONT_SCALE      = new ConfigDouble("chunkUnloadBucketOverlayFontScale", 0.1625, "The font scale for the Chunk unload order bucket overlay.\nValid range: 0.01 - 1.0");
        public static final ConfigInteger       CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS  = new ConfigInteger("chunkUnloadBucketOverlayChunkRadius", -1, "The radius of chunks to render the text for in the overlay.\nValid range: -1 - 40, where -1 = render distance");
        public static final ConfigString        COORDINATE_FORMAT_STRING            = new ConfigString("coordinateFormat", "x: %.1f y: %.1f z: %.1f", "The format string for the coordinate line.\nNeeds to have three %f format strings! Default: x: %.1f y: %.1f z: %.1f");
        public static final ConfigString        DATE_FORMAT_REAL                    = new ConfigString("dateFormatReal", "yyyy-MM-dd HH:mm:ss", "The format string for real time, see the Java SimpleDateFormat\nclass for the format patterns, if needed.");
        public static final ConfigString        DATE_FORMAT_MINECRAFT               = new ConfigString("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx", "The format string for the Minecraft time.\nThe supported placeholders are: {DAY}, {HOUR}, {MIN},;{SEC}");
        public static final ConfigBoolean       DEBUG_RENDERER_PATH_MAX_DIST        = new ConfigBoolean("debugRendererPathFindingEnablePointWidth", true, "If true, then the vanilla pathfinding debug renderer will render the path point width boxes.");
        public static final ConfigBoolean       ENABLE_BY_DEFAULT                   = new ConfigBoolean("enableByDefault", true, "If true, the HUD will be enabled by default on game launch");
        public static final ConfigBoolean       FIX_VANILLA_DEBUG_RENDERERS         = new ConfigBoolean("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed.");
        public static final ConfigColor         FONT_COLOR                          = new ConfigColor("fontColor", "0xE0E0E0", "Font color (RGB, default: 0xE0E0E0 = 14737632)");
        public static final ConfigDouble        FONT_SCALE                          = new ConfigDouble("fontScale", 0.5, "Font scale factor. Valid range: 0.0 - 10.0. Default: 0.5\n");
        public static final ConfigOptionList    HUD_ALIGNMENT                       = new ConfigOptionList("hudAlignment", HudAlignment.TOP_LEFT, "The alignment of the HUD.");
        public static final ConfigColor         REGION_OVERLAY_COLOR                = new ConfigColor("regionOverlayColor", "0xFFFF8019", "Color for the region file overlay (ARGB, default: 0xFFFF8019)");
        public static final ConfigBoolean       REQUIRE_SNEAK                       = new ConfigBoolean("requireSneak", false, "Require the player to be sneaking to render the HUD");
        public static final ConfigHotkey        REQUIRED_KEY                        = new ConfigHotkey("requiredKey", "", "Require holding this key to render the HUD");
        public static final ConfigBoolean       SORT_LINES_BY_LENGTH                = new ConfigBoolean("sortLinesByLength", false, "Sort the lines by their text's length");
        public static final ConfigBoolean       SORT_LINES_REVERSED                 = new ConfigBoolean("sortLinesReversed", false, "Reverse the line sorting order");
        public static final ConfigColor         SLIME_CHUNKS_OVERLAY_COLOR          = new ConfigColor("slimeChunksOverlayColor", "0xFF20F020", "Color for the slime chunks overlay (ARGB, default: 0xFF2050D0)");
        public static final ConfigInteger       SLIME_CHUNK_OVERLAY_RADIUS          = new ConfigInteger("slimeChunkOverlayRadius", -1, "The radius of chunks to render the slime chunk overlay in.\nValid range: 0 - 40, where -1 = render distance");
        public static final ConfigColor         SPAWN_PLAYER_ENTITY_OVERLAY_COLOR   = new ConfigColor("spawnPlayerEntityOverlayColor", "0xFF2050D0", "Color for the entity-processing spawn chunks overlay of\nhow the spawn chunks would be if the spawn were to be;at the player's\ncurrent position (ARGB, default: 0xFF2050D0)");
        public static final ConfigColor         SPAWN_PLAYER_LAZY_OVERLAY_COLOR     = new ConfigColor("spawnPlayerLazyOverlayColor", "0xFFD030D0", "Color for the \"lazy-loaded\" spawn chunks overlay of\nhow the spawn chunks would be if the spawn were to be at the;player's\ncurrent position(ARGB, default: 0xFFD030D0)");
        public static final ConfigColor         SPAWN_REAL_ENTITY_OVERLAY_COLOR     = new ConfigColor("spawnRealEntityOverlayColor", "0xFF30FF20", "Color for the entity-processing real spawn chunks overlay (ARGB, default: 0xFF30FF20)");
        public static final ConfigColor         SPAWN_REAL_LAZY_OVERLAY_COLOR       = new ConfigColor("spawnRealLazyOverlayColor", "0xFFFF3020", "Color for the \"lazy-loaded\" real spawn chunks overlay (ARGB, default: 0xFFFF3020)");
        public static final ConfigColor         TEXT_BACKGROUND_COLOR               = new ConfigColor("textBackgroundColor", "0xA0505050", "Text background color (ARGB, default: 0xA0505050)");
        public static final ConfigInteger       TEXT_POS_X                          = new ConfigInteger("textPosX", 4, "Text X position from the screen edge (default: 4)");
        public static final ConfigInteger       TEXT_POS_Y                          = new ConfigInteger("textPosY", 4, "Text Y position from the screen edge (default: 4)");
        public static final ConfigBoolean       USE_CUSTOMIZED_COORDINATES          = new ConfigBoolean("useCustomizedCoordinateFormat", true, "Use the customized coordinate format string");
        public static final ConfigBoolean       USE_FONT_SHADOW                     = new ConfigBoolean("useFontShadow", false, "Use font shadow");
        public static final ConfigBoolean       USE_TEXT_BACKGROUND                 = new ConfigBoolean("useTextBackground", true, "Use a solid background color behind the text");

        public static final ImmutableList<IConfigValue> OPTIONS = ImmutableList.of(
                TOGGLE_KEY,
                REQUIRED_KEY,

                CHUNK_UNLOAD_BUCKET_FONT_SCALE,
                CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS,
                COORDINATE_FORMAT_STRING,
                DATE_FORMAT_REAL,
                DATE_FORMAT_MINECRAFT,
                DEBUG_RENDERER_PATH_MAX_DIST,
                ENABLE_BY_DEFAULT,
                FIX_VANILLA_DEBUG_RENDERERS,
                FONT_COLOR,
                FONT_SCALE,
                HUD_ALIGNMENT,
                REGION_OVERLAY_COLOR,
                REQUIRE_SNEAK,
                SORT_LINES_BY_LENGTH,
                SORT_LINES_REVERSED,
                SLIME_CHUNKS_OVERLAY_COLOR,
                SLIME_CHUNK_OVERLAY_RADIUS,
                SPAWN_PLAYER_ENTITY_OVERLAY_COLOR,
                SPAWN_PLAYER_LAZY_OVERLAY_COLOR,
                SPAWN_REAL_ENTITY_OVERLAY_COLOR,
                SPAWN_REAL_LAZY_OVERLAY_COLOR,
                TEXT_BACKGROUND_COLOR,
                TEXT_POS_X,
                TEXT_POS_Y,
                USE_CUSTOMIZED_COORDINATES,
                USE_FONT_SHADOW,
                USE_TEXT_BACKGROUND
                );
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
                JsonObject objRendererHotkeys   = JsonUtils.getNestedObject(root, "RendererHotkeys", false);
                JsonObject objRendererToggles   = JsonUtils.getNestedObject(root, "RendererToggles", false);

                ConfigUtils.readConfigValues(root, "Generic", Configs.Generic.OPTIONS);

                for (InfoToggle toggle : InfoToggle.values())
                {
                    if (objToggles != null && JsonUtils.hasBoolean(objToggles, toggle.getName()))
                    {
                        toggle.setBooleanValue(JsonUtils.getBoolean(objToggles, toggle.getName()));
                    }

                    if (objInfoHotkeys != null && JsonUtils.hasString(objInfoHotkeys, toggle.getName()))
                    {
                        toggle.getKeybind().setValueFromString(JsonUtils.getString(objInfoHotkeys, toggle.getName()));
                    }

                    if (objInfoLineOrders != null && JsonUtils.hasInteger(objInfoLineOrders, toggle.getName()))
                    {
                        toggle.setIntegerValue(JsonUtils.getInteger(objInfoLineOrders, toggle.getName()));
                    }
                }

                for (RendererToggle hotkey : RendererToggle.values())
                {
                    if (objRendererToggles != null && JsonUtils.hasBoolean(objRendererToggles, hotkey.getName()))
                    {
                        hotkey.setBooleanValue(JsonUtils.getBoolean(objRendererToggles, hotkey.getName()));
                    }

                    if (objRendererHotkeys != null && JsonUtils.hasString(objRendererHotkeys, hotkey.getName()))
                    {
                        hotkey.getKeybind().setValueFromString(JsonUtils.getString(objRendererHotkeys, hotkey.getName()));
                    }
                }
            }
        }

        RenderEventHandler.getInstance().setFontScale(Configs.Generic.FONT_SCALE.getDoubleValue());
    }

    public static void save()
    {
        File dir = LiteLoader.getCommonConfigFolder();

        if (dir.exists() && dir.isDirectory())
        {
            JsonObject root = new JsonObject();

            JsonObject objToggles           = JsonUtils.getNestedObject(root, "InfoTypeToggles", true);
            JsonObject objInfoHotkeys       = JsonUtils.getNestedObject(root, "InfoTypeHotkeys", true);
            JsonObject objInfoLineOrders    = JsonUtils.getNestedObject(root, "InfoLineOrders", true);
            JsonObject objRendererHotkeys   = JsonUtils.getNestedObject(root, "RendererHotkeys", true);
            JsonObject objRendererToggles   = JsonUtils.getNestedObject(root, "RendererToggles", true);

            ConfigUtils.writeConfigValues(root, "Generic", Configs.Generic.OPTIONS);

            for (InfoToggle toggle : InfoToggle.values())
            {
                objToggles.add(toggle.getName(), new JsonPrimitive(toggle.getBooleanValue()));
                objInfoHotkeys.add(toggle.getName(), new JsonPrimitive(toggle.getKeybind().getStringValue()));
                objInfoLineOrders.add(toggle.getName(), new JsonPrimitive(toggle.getIntegerValue()));
            }

            for (RendererToggle hotkey : RendererToggle.values())
            {
                objRendererToggles.add(hotkey.getName(), new JsonPrimitive(hotkey.getBooleanValue()));
                objRendererHotkeys.add(hotkey.getName(), new JsonPrimitive(hotkey.getKeybind().getStringValue()));
            }

            JsonUtils.writeJsonToFile(root, new File(dir, CONFIG_FILE_NAME));
        }
    }
}
