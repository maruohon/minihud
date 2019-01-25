package fi.dy.masa.minihud.config;

import java.io.File;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.mumfrey.liteloader.core.LiteLoader;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.HudAlignment;
import fi.dy.masa.malilib.config.IConfigBase;
import fi.dy.masa.malilib.config.IConfigHandler;
import fi.dy.masa.malilib.config.IConfigValue;
import fi.dy.masa.malilib.config.options.ConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigColor;
import fi.dy.masa.malilib.config.options.ConfigDouble;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.config.options.ConfigInteger;
import fi.dy.masa.malilib.config.options.ConfigOptionList;
import fi.dy.masa.malilib.config.options.ConfigString;
import fi.dy.masa.malilib.hotkeys.KeybindSettings;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.event.RenderHandler;
import fi.dy.masa.minihud.util.BlockGridMode;

public class Configs implements IConfigHandler
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";
    private static final int CONFIG_VERSION = 1;

    public static class Generic
    {
        public static final ConfigOptionList    BLOCK_GRID_OVERLAY_MODE             = new ConfigOptionList("blockGridOverlayMode", BlockGridMode.ALL, "The block grid render mode");
        public static final ConfigInteger       BLOCK_GRID_OVERLAY_RADIUS           = new ConfigInteger("blockGridOverlayRadius", 32, "The radius of the block grid lines to render");
        public static final ConfigDouble        CHUNK_UNLOAD_BUCKET_FONT_SCALE      = new ConfigDouble("chunkUnloadBucketOverlayFontScale", 0.1625, "The font scale for the Chunk unload order bucket overlay.\nValid range: 0.01 - 1.0");
        public static final ConfigInteger       CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS  = new ConfigInteger("chunkUnloadBucketOverlayChunkRadius", -1, "The radius of chunks to render the text for in the overlay.\nValid range: -1 - 40, where -1 = render distance");
        public static final ConfigBoolean       CHUNK_UNLOAD_BUCKET_WITH_SIZE       = new ConfigBoolean("chunkUnloadBucketWithSize", false, "If enabled, uses the more accurate (but still experimental)\nchunk unload bucket calculations, taken from the Carpet mod");
        public static final ConfigString        COORDINATE_FORMAT_STRING            = new ConfigString("coordinateFormat", "x: %.1f y: %.1f z: %.1f", "The format string for the coordinate line.\nNeeds to have three %f format strings!\nDefault: x: %.1f y: %.1f z: %.1f");
        public static final ConfigString        DATE_FORMAT_REAL                    = new ConfigString("dateFormatReal", "yyyy-MM-dd HH:mm:ss", "The format string for real time, see the Java SimpleDateFormat\nclass for the format patterns, if needed.");
        public static final ConfigString        DATE_FORMAT_MINECRAFT               = new ConfigString("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx", "The format string for the Minecraft time.\nThe supported placeholders are: {DAY}, {HOUR}, {MIN},;{SEC}");
        public static final ConfigBoolean       DEBUG_RENDERER_PATH_MAX_DIST        = new ConfigBoolean("debugRendererPathFindingEnablePointWidth", true, "If true, then the vanilla pathfinding debug renderer\nwill render the path point width boxes.");
        public static final ConfigBoolean       DESPAWN_SPHERE_ROUND                = new ConfigBoolean("despawnSpehereRound", false, "Use a round despawn sphere instead of a \"block sphere\"");
        public static final ConfigBoolean       ENABLED                             = new ConfigBoolean("enabled", true, "If true, the HUD will be rendered");
        public static final ConfigBoolean       FIX_VANILLA_DEBUG_RENDERERS         = new ConfigBoolean("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed.");
        public static final ConfigDouble        FONT_SCALE                          = new ConfigDouble("fontScale", 0.5, "Font scale factor. Valid range: 0.0 - 10.0. Default: 0.5\n");
        public static final ConfigOptionList    HUD_ALIGNMENT                       = new ConfigOptionList("hudAlignment", HudAlignment.TOP_LEFT, "The alignment of the HUD.");
        public static final ConfigHotkey        OPEN_CONFIG_GUI                     = new ConfigHotkey("openConfigGui", "H,C", "A hotkey to open the in-game Config GUI");
        public static final ConfigBoolean       REQUIRE_SNEAK                       = new ConfigBoolean("requireSneak", false, "Require the player to be sneaking to render the HUD");
        public static final ConfigHotkey        REQUIRED_KEY                        = new ConfigHotkey("requiredKey", "", KeybindSettings.MODIFIER_INGAME_EMPTY, "Require holding this key to render the HUD");
        public static final ConfigBoolean       SORT_LINES_BY_LENGTH                = new ConfigBoolean("sortLinesByLength", false, "Sort the lines by their text's length");
        public static final ConfigBoolean       SORT_LINES_REVERSED                 = new ConfigBoolean("sortLinesReversed", false, "Reverse the line sorting order");
        public static final ConfigInteger       SLIME_CHUNK_OVERLAY_RADIUS          = new ConfigInteger("slimeChunkOverlayRadius", -1, "The radius of chunks to render the slime chunk overlay in.\nValid range: 0 - 40, where -1 = render distance");
        public static final ConfigInteger       SPAWNABLE_COLUMNS_OVERLAY_RADIUS    = new ConfigInteger("spawnableColumnHeightsOverlayRadius", 40, 0, 128, "The radius (in blocks) to render the spawnable\ncolumn heights overlay in. Valid range: 0 - 128");
        public static final ConfigInteger       SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL  = new ConfigInteger("spawnableSubChunkCheckInterval", 20, 1, 10000, "The interval in game ticks for the spawnable sub-chunk heightmap checks");
        public static final ConfigInteger       SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS = new ConfigInteger("spawnableSubChunksOverlayRadius", -1, "The radius of chunks to render the spawnable sub-chunks\noverlay in. Valid range: -1 - 40, where -1 = render distance");
        public static final ConfigInteger       TEXT_POS_X                          = new ConfigInteger("textPosX", 4, "Text X position from the screen edge (default: 4)");
        public static final ConfigInteger       TEXT_POS_Y                          = new ConfigInteger("textPosY", 4, "Text Y position from the screen edge (default: 4)");
        public static final ConfigHotkey        TOGGLE_KEY                          = new ConfigHotkey("toggleKey", "H", KeybindSettings.RELEASE_EXCLUSIVE, "The main toggle key");
        public static final ConfigBoolean       USE_CUSTOMIZED_COORDINATES          = new ConfigBoolean("useCustomizedCoordinateFormat", true, "Use the customized coordinate format string");
        public static final ConfigBoolean       USE_FONT_SHADOW                     = new ConfigBoolean("useFontShadow", false, "Use font shadow");
        public static final ConfigBoolean       USE_TEXT_BACKGROUND                 = new ConfigBoolean("useTextBackground", true, "Use a solid background color behind the text");

        public static final ImmutableList<IConfigBase> OPTIONS = ImmutableList.of(
                ENABLED,
                CHUNK_UNLOAD_BUCKET_WITH_SIZE,
                DEBUG_RENDERER_PATH_MAX_DIST,
                DESPAWN_SPHERE_ROUND,
                FIX_VANILLA_DEBUG_RENDERERS,
                REQUIRE_SNEAK,
                SORT_LINES_BY_LENGTH,
                SORT_LINES_REVERSED,
                USE_CUSTOMIZED_COORDINATES,
                USE_FONT_SHADOW,
                USE_TEXT_BACKGROUND,

                TOGGLE_KEY,
                REQUIRED_KEY,
                OPEN_CONFIG_GUI,
                BLOCK_GRID_OVERLAY_MODE,
                HUD_ALIGNMENT,

                BLOCK_GRID_OVERLAY_RADIUS,
                CHUNK_UNLOAD_BUCKET_FONT_SCALE,
                CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS,
                COORDINATE_FORMAT_STRING,
                DATE_FORMAT_REAL,
                DATE_FORMAT_MINECRAFT,
                FONT_SCALE,
                SLIME_CHUNK_OVERLAY_RADIUS,
                SPAWNABLE_COLUMNS_OVERLAY_RADIUS,
                SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL,
                SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS,
                TEXT_POS_X,
                TEXT_POS_Y
        );
    }

    public static class Colors
    {
        public static final ConfigColor BLOCK_GRID_OVERLAY_COLOR            = new ConfigColor("blockGridOverlayColor", "0x80FFFFFF", "Color for the block grid overlay");
        public static final ConfigColor DESPAWN_SPHERE_OVERLAY_COLOR        = new ConfigColor("despawnSphereOverlayColor", "0x80A04050", "Color for the despawn sphere overlay");
        public static final ConfigColor RANDOM_TICKS_FIXED_OVERLAY_COLOR    = new ConfigColor("randomTicksFixedOverlayColor", "0xFFF9F225", "Color for the fixed-point random ticked chunks overlay");
        public static final ConfigColor RANDOM_TICKS_PLAYER_OVERLAY_COLOR   = new ConfigColor("randomTicksPlayerOverlayColor", "0xFF30FE73", "Color for the player-following random ticked chunks overlay");
        public static final ConfigColor REGION_OVERLAY_COLOR                = new ConfigColor("regionOverlayColor", "0xFFFF8019", "Color for the region file overlay");
        public static final ConfigColor SLIME_CHUNKS_OVERLAY_COLOR          = new ConfigColor("slimeChunksOverlayColor", "0xFF20F020", "Color for the slime chunks overlay");
        public static final ConfigColor SPAWN_PLAYER_ENTITY_OVERLAY_COLOR   = new ConfigColor("spawnPlayerEntityOverlayColor", "0xFF2050D0", "Color for the entity-processing spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_PLAYER_LAZY_OVERLAY_COLOR     = new ConfigColor("spawnPlayerLazyOverlayColor", "0xFFD030D0", "Color for the \"lazy-loaded\" spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_REAL_ENTITY_OVERLAY_COLOR     = new ConfigColor("spawnRealEntityOverlayColor", "0xFF30FF20", "Color for the entity-processing real spawn chunks overlay");
        public static final ConfigColor SPAWN_REAL_LAZY_OVERLAY_COLOR       = new ConfigColor("spawnRealLazyOverlayColor", "0xFFFF3020", "Color for the \"lazy-loaded\" real spawn chunks overlay");
        public static final ConfigColor SPAWNABLE_CHUNKS_FIXED_OVERLAY_COLOR    = new ConfigColor("spawnableChunkFixedOverlayColor", "0xFFFF2090", "Color for the location-fixed spawnable chunks overlay");
        public static final ConfigColor SPAWNABLE_CHUNKS_PLAYER_OVERLAY_COLOR   = new ConfigColor("spawnableChunksPlayerOverlayColor", "0xFFFF3030", "Color for the player-following spawnable chunks overlay");
        public static final ConfigColor SPAWNABLE_COLUMNS_OVERLAY_COLOR     = new ConfigColor("spawnableColumnHeightsOverlayColor", "0xA0FF00FF", "Color for the spawnable sub-chunks overlay");
        public static final ConfigColor SPAWNABLE_SUB_CHUNKS_OVERLAY_COLOR  = new ConfigColor("spawnableSubChunksOverlayColor", "0xFF2050D0", "Color for the spawnable sub-chunks overlay");
        public static final ConfigColor TEXT_BACKGROUND_COLOR               = new ConfigColor("textBackgroundColor", "0xA0505050", "Text background color");
        public static final ConfigColor TEXT_COLOR                          = new ConfigColor("textColor", "0xE0E0E0", "Info line text color");

        public static final ImmutableList<IConfigValue> OPTIONS = ImmutableList.of(
                BLOCK_GRID_OVERLAY_COLOR,
                DESPAWN_SPHERE_OVERLAY_COLOR,
                RANDOM_TICKS_FIXED_OVERLAY_COLOR,
                RANDOM_TICKS_PLAYER_OVERLAY_COLOR,
                REGION_OVERLAY_COLOR,
                SLIME_CHUNKS_OVERLAY_COLOR,
                SPAWN_PLAYER_ENTITY_OVERLAY_COLOR,
                SPAWN_PLAYER_LAZY_OVERLAY_COLOR,
                SPAWN_REAL_ENTITY_OVERLAY_COLOR,
                SPAWN_REAL_LAZY_OVERLAY_COLOR,
                SPAWNABLE_CHUNKS_FIXED_OVERLAY_COLOR,
                SPAWNABLE_CHUNKS_PLAYER_OVERLAY_COLOR,
                SPAWNABLE_COLUMNS_OVERLAY_COLOR,
                SPAWNABLE_SUB_CHUNKS_OVERLAY_COLOR,
                TEXT_BACKGROUND_COLOR,
                TEXT_COLOR
        );
    }

    public static void loadFromFile()
    {
        File configFile = new File(LiteLoader.getCommonConfigFolder(), CONFIG_FILE_NAME);

        if (configFile.exists() && configFile.isFile() && configFile.canRead())
        {
            JsonElement element = JsonUtils.parseJsonFile(configFile);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();
                JsonObject objInfoLineOrders = JsonUtils.getNestedObject(root, "InfoLineOrders", false);

                ConfigUtils.readConfigBase(root, "Generic", Configs.Generic.OPTIONS);
                ConfigUtils.readConfigBase(root, "Colors", Configs.Colors.OPTIONS);
                ConfigUtils.readHotkeyToggleOptions(root, "RendererHotkeys", "RendererToggles", ImmutableList.copyOf(RendererToggle.values()));
                ConfigUtils.readHotkeyToggleOptions(root, "InfoHotkeys", "InfoTypeToggles", ImmutableList.copyOf(InfoToggle.values()));
                int version = JsonUtils.getIntegerOrDefault(root, "config_version", 0);

                if (objInfoLineOrders != null && version >= 1)
                {
                    for (InfoToggle toggle : InfoToggle.values())
                    {
                        if (JsonUtils.hasInteger(objInfoLineOrders, toggle.getName()))
                        {
                            toggle.setIntegerValue(JsonUtils.getInteger(objInfoLineOrders, toggle.getName()));
                        }
                    }
                }
            }
        }

        RenderHandler.getInstance().setFontScale(Configs.Generic.FONT_SCALE.getDoubleValue());
    }

    public static void saveToFile()
    {
        File dir = LiteLoader.getCommonConfigFolder();

        if (dir.exists() && dir.isDirectory())
        {
            JsonObject root = new JsonObject();
            JsonObject objInfoLineOrders = JsonUtils.getNestedObject(root, "InfoLineOrders", true);

            ConfigUtils.writeConfigBase(root, "Generic", Configs.Generic.OPTIONS);
            ConfigUtils.writeConfigBase(root, "Colors", Configs.Colors.OPTIONS);
            ConfigUtils.writeHotkeyToggleOptions(root, "RendererHotkeys", "RendererToggles", ImmutableList.copyOf(RendererToggle.values()));
            ConfigUtils.writeHotkeyToggleOptions(root, "InfoHotkeys", "InfoTypeToggles", ImmutableList.copyOf(InfoToggle.values()));

            for (InfoToggle toggle : InfoToggle.values())
            {
                objInfoLineOrders.add(toggle.getName(), new JsonPrimitive(toggle.getIntegerValue()));
            }

            root.add("config_version", new JsonPrimitive(CONFIG_VERSION));

            JsonUtils.writeJsonToFile(root, new File(dir, CONFIG_FILE_NAME));
        }
    }

    @Override
    public void onConfigsChanged()
    {
        saveToFile();
        loadFromFile();
    }

    @Override
    public void save()
    {
        saveToFile();
    }
}
