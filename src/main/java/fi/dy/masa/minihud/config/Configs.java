package fi.dy.masa.minihud.config;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.IConfigHandler;
import fi.dy.masa.malilib.config.options.ConfigBoolean;
import fi.dy.masa.malilib.config.options.ConfigColor;
import fi.dy.masa.malilib.config.options.ConfigDouble;
import fi.dy.masa.malilib.config.options.ConfigHotkey;
import fi.dy.masa.malilib.config.options.ConfigInteger;
import fi.dy.masa.malilib.config.options.ConfigOptionList;
import fi.dy.masa.malilib.config.options.ConfigString;
import fi.dy.masa.malilib.config.options.IConfigBase;
import fi.dy.masa.malilib.config.options.IConfigValue;
import fi.dy.masa.malilib.config.values.HudAlignment;
import fi.dy.masa.malilib.hotkeys.KeybindSettings;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.util.BlockGridMode;
import fi.dy.masa.minihud.util.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.LightLevelNumberMode;

public class Configs implements IConfigHandler
{
    public static class Generic
    {
        public static final ConfigOptionList    BLOCK_GRID_OVERLAY_MODE             = new ConfigOptionList("blockGridOverlayMode", BlockGridMode.ALL, "The block grid render mode");
        public static final ConfigInteger       BLOCK_GRID_OVERLAY_RADIUS           = new ConfigInteger("blockGridOverlayRadius", 32, "The radius of the block grid lines to render");
        public static final ConfigDouble        CHUNK_UNLOAD_BUCKET_FONT_SCALE      = new ConfigDouble("chunkUnloadBucketOverlayFontScale", 0.1625, "The font scale for the Chunk unload order bucket overlay.\nValid range: 0.01 - 1.0");
        public static final ConfigInteger       DROPPED_CHUNKS_HASH_SIZE            = new ConfigInteger("droppedChunksHashSize", -1, -1, Integer.MAX_VALUE, "The HashSet size for the chunk unload bucket calculation,\nif 'chunkUnloadBucketWithSize' is true.\nUse -1 for automatically getting the current value in single player.");
        public static final ConfigInteger       CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS  = new ConfigInteger("chunkUnloadBucketOverlayChunkRadius", -1, "The radius of chunks to render the text for in the overlay.\nValid range: -1 - 40, where -1 = render distance");
        public static final ConfigBoolean       CHUNK_UNLOAD_BUCKET_WITH_SIZE       = new ConfigBoolean("chunkUnloadBucketWithSize", false, "If enabled, uses the more accurate (but still experimental)\nchunk unload bucket calculations, taken from the Carpet mod");
        public static final ConfigString        COORDINATE_FORMAT_STRING            = new ConfigString("coordinateFormat", "x: %.1f y: %.1f z: %.1f", "The format string for the coordinate line ('infoCoordinates').\nNeeds to have three %f format strings!\nDefault: x: %.1f y: %.1f z: %.1f");
        public static final ConfigString        DATE_FORMAT_REAL                    = new ConfigString("dateFormatReal", "yyyy-MM-dd HH:mm:ss", "The format string for real time, see the Java SimpleDateFormat\nclass for the format patterns, if needed.");
        public static final ConfigString        DATE_FORMAT_MINECRAFT               = new ConfigString("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx", "The format string for the Minecraft time.\nThe supported placeholders are: {DAY_1}, {DAY}, {HOUR}, {MIN}, {SEC}.\n{DAY_1} starts the day counter from 1, {DAY} starts from 0.");
        public static final ConfigBoolean       DEBUG_RENDERER_PATH_MAX_DIST        = new ConfigBoolean("debugRendererPathFindingEnablePointWidth", true, "If true, then the vanilla pathfinding debug renderer\nwill render the path point width boxes.");
        public static final ConfigBoolean       DONT_RESET_SEED_ON_DIMENSION_CHANGE = new ConfigBoolean("dontResetSeedOnDimensionChange", false, "Don't reset the world seed when just changing dimensions.\nSome mods may use per-dimension seeds, so by default the seed\nis reset every time the player changes dimensions.");
        public static final ConfigBoolean       ENABLED                             = new ConfigBoolean("enabled", true, "The main rendering toggle for all MiniHUD rendering");
        public static final ConfigBoolean       FIX_VANILLA_DEBUG_RENDERERS         = new ConfigBoolean("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed.");
        public static final ConfigDouble        FONT_SCALE                          = new ConfigDouble("fontScale", 0.5, 0.0, 100.0, "Font scale factor for the info line HUD. Default: 0.5\n");
        public static final ConfigOptionList    HUD_ALIGNMENT                       = new ConfigOptionList("hudAlignment", HudAlignment.TOP_LEFT, "The alignment of the info line HUD");
        public static final ConfigBoolean       LIGHT_LEVEL_COLORED_NUMBERS         = new ConfigBoolean("lightLevelColoredNumbers", true, "Whether to use colored or white numbers\nfor the Light Level overlay numbers");
        public static final ConfigOptionList    LIGHT_LEVEL_MARKER_MODE             = new ConfigOptionList("lightLevelMarkers", LightLevelMarkerMode.SQUARE, "Which type of colored marker to use in the\nLight Level overlay, if any");
        public static final ConfigDouble        LIGHT_LEVEL_MARKER_SIZE             = new ConfigDouble("lightLevelMarkerSize", 0.84, 0.0, 1.0, "The size of the light level colored marker.\nRange: 0.0 - 1.0");
        public static final ConfigOptionList    LIGHT_LEVEL_NUMBER_MODE             = new ConfigOptionList("lightLevelNumbers", LightLevelNumberMode.BLOCK, "Which light level number(s) to render in the Light Level overlay");
        public static final ConfigDouble        LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X   = new ConfigDouble("lightLevelNumberOffsetBlockX", 0.09, 0.0, 1.0, "The relative \"x\" offset for the block light level number.\nRange: 0.0 - 1.0");
        public static final ConfigDouble        LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y   = new ConfigDouble("lightLevelNumberOffsetBlockY", 0.12, 0.0, 1.0, "The relative \"y\" offset for the block light level number.\nRange: 0.0 - 1.0");
        public static final ConfigDouble        LIGHT_LEVEL_NUMBER_OFFSET_SKY_X     = new ConfigDouble("lightLevelNumberOffsetSkyX", 0.42, 0.0, 1.0, "The relative \"x\" offset for the sky light level number.\nRange: 0.0 - 1.0");
        public static final ConfigDouble        LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y     = new ConfigDouble("lightLevelNumberOffsetSkyY", 0.56, 0.0, 1.0, "The relative \"y\" offset for the sky light level number.\nRange: 0.0 - 1.0");
        public static final ConfigBoolean       LIGHT_LEVEL_NUMBER_ROTATION         = new ConfigBoolean("lightLevelNumberRotation", true, "If true, then the light level numbers will rotate\naccording to the player's current facing");
        public static final ConfigInteger       LIGHT_LEVEL_RANGE                   = new ConfigInteger("lightLevelRange", 24, 1, 64, "The block range to render the Light Level overlay in");
        public static final ConfigInteger       LIGHT_LEVEL_THRESHOLD               = new ConfigInteger("lightLevelThreshold", 8, 0, 15, "The light level threshold which is considered safe");
        public static final ConfigDouble        LIGHT_LEVEL_Z_OFFSET                = new ConfigDouble("lightLevelZOffset", 0.005, 0.0, 1.0, "The relative \"z\" offset for the light level overlay.\nMeant to help with potential z-fighting issues.\nRange: 0.0 - 1.0");
        public static final ConfigBoolean       MAP_PREVIEW                         = new ConfigBoolean("mapPreview", false, "Enables rendering a preview of the map,\nwhen you hold shift while hovering over a map item");
        public static final ConfigInteger       MAP_PREVIEW_SIZE                    = new ConfigInteger("mapPreviewSize", 160, 16, 512, "The size of the rendered map previews");
        public static final ConfigHotkey        OPEN_CONFIG_GUI                     = new ConfigHotkey("openConfigGui", "H,C", "A hotkey to open the in-game Config GUI");
        public static final ConfigBoolean       REQUIRE_SNEAK                       = new ConfigBoolean("requireSneak", false, "Require the player to be sneaking to render the info line HUD");
        public static final ConfigHotkey        REQUIRED_KEY                        = new ConfigHotkey("requiredKey", "", KeybindSettings.MODIFIER_INGAME_EMPTY, "Require holding this key to render the HUD");
        public static final ConfigHotkey        SET_DISTANCE_REFERENCE_POINT        = new ConfigHotkey("setDistanceReferencePoint", "", "A hotkey to store the player's current position\nas the reference point for the distance info line type");
        public static final ConfigHotkey        SHAPE_EDITOR                        = new ConfigHotkey("shapeEditor", "", "Opens the Shape Editor GUI for the selected shape");
        public static final ConfigBoolean       SHULKER_BOX_PREVIEW                 = new ConfigBoolean("shulkerBoxPreview", false, "Enables rendering a preview of the Shulker Box contents,\nwhen you hold shift while hovering over a Shulker Box item");
        public static final ConfigBoolean       SHULKER_DISPLAY_BACKGROUND_COLOR    = new ConfigBoolean("shulkerDisplayBgColor", true, "Enables tinting/coloring the Shulker Box display\nbackground texture with the dye color of the box");
        public static final ConfigBoolean       SHULKER_DISPLAY_REQUIRE_SHIFT       = new ConfigBoolean("shulkerDisplayRequireShift", true, "Whether or not holding shift is required for the Shulker Box preview");
        public static final ConfigInteger       SLIME_CHUNK_OVERLAY_RADIUS          = new ConfigInteger("slimeChunkOverlayRadius", -1, -1, 40, "The radius of chunks to render the slime chunk overlay in.\nValid range: -1 ... 40, where -1 = render distance");
        public static final ConfigBoolean       SORT_LINES_BY_LENGTH                = new ConfigBoolean("sortLinesByLength", false, "Sort the lines by their text's length");
        public static final ConfigBoolean       SORT_LINES_REVERSED                 = new ConfigBoolean("sortLinesReversed", false, "Reverse the line sorting order");
        public static final ConfigInteger       SPAWNABLE_COLUMNS_OVERLAY_RADIUS    = new ConfigInteger("spawnableColumnHeightsOverlayRadius", 40, 0, 128, "The radius (in blocks) to render the spawnable\ncolumn heights overlay in. Valid range: 0 ... 128");
        public static final ConfigInteger       SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL  = new ConfigInteger("spawnableSubChunkCheckInterval", 20, 1, 10000, "The interval in game ticks for the spawnable sub-chunk heightmap checks");
        public static final ConfigInteger       SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS = new ConfigInteger("spawnableSubChunksOverlayRadius", -1, -1, 40, "The radius of chunks to render the spawnable sub-chunks\noverlay in. Valid range: -1 ... 40, where -1 = render distance");
        public static final ConfigBoolean       STRUCTURES_RENDER_THROUGH           = new ConfigBoolean("structuresRenderThrough", false, "If enabled, then the Structure Bounding Boxes\nwill be rendered through blocks");
        public static final ConfigInteger       TEXT_POS_X                          = new ConfigInteger("textPosX", 4, "Info line text X position from the screen edge (default: 4)");
        public static final ConfigInteger       TEXT_POS_Y                          = new ConfigInteger("textPosY", 4, "Info line text Y position from the screen edge (default: 4)");
        public static final ConfigInteger       TIME_DAY_DIVISOR                    = new ConfigInteger("timeDayDivisor", 24000, 1, Integer.MAX_VALUE, "The divisor value for the modulo of the day time,\nused for 'infoTimeDayModulo' info line");
        public static final ConfigInteger       TIME_TOTAL_DIVISOR                  = new ConfigInteger("timeTotalDivisor", 24000, 1, Integer.MAX_VALUE, "The divisor value for the modulo of the total world time,\nused for 'infoTimeTotalModulo' info line");
        public static final ConfigHotkey        TOGGLE_KEY                          = new ConfigHotkey("toggleKey", "H", KeybindSettings.RELEASE_EXCLUSIVE, "The main rendering toggle key");
        public static final ConfigBoolean       USE_CUSTOMIZED_COORDINATES          = new ConfigBoolean("useCustomizedCoordinateFormat", true, "Use the customized coordinate format string from 'coordinateFormat'");
        public static final ConfigBoolean       USE_FONT_SHADOW                     = new ConfigBoolean("useFontShadow", false, "Use font shadow for the info line HUD text");
        public static final ConfigBoolean       USE_TEXT_BACKGROUND                 = new ConfigBoolean("useTextBackground", true, "Use a solid background color behind the text\nfor the info line HUD text");

        public static final ImmutableList<IConfigBase> OPTIONS = ImmutableList.of(
                ENABLED,
                CHUNK_UNLOAD_BUCKET_WITH_SIZE,
                DEBUG_RENDERER_PATH_MAX_DIST,
                DONT_RESET_SEED_ON_DIMENSION_CHANGE,
                FIX_VANILLA_DEBUG_RENDERERS,
                LIGHT_LEVEL_COLORED_NUMBERS,
                LIGHT_LEVEL_NUMBER_ROTATION,
                MAP_PREVIEW,
                REQUIRE_SNEAK,
                SHULKER_BOX_PREVIEW,
                SHULKER_DISPLAY_BACKGROUND_COLOR,
                SHULKER_DISPLAY_REQUIRE_SHIFT,
                SORT_LINES_BY_LENGTH,
                SORT_LINES_REVERSED,
                STRUCTURES_RENDER_THROUGH,
                USE_CUSTOMIZED_COORDINATES,
                USE_FONT_SHADOW,
                USE_TEXT_BACKGROUND,

                OPEN_CONFIG_GUI,
                REQUIRED_KEY,
                SET_DISTANCE_REFERENCE_POINT,
                SHAPE_EDITOR,
                TOGGLE_KEY,

                BLOCK_GRID_OVERLAY_MODE,
                LIGHT_LEVEL_MARKER_MODE,
                LIGHT_LEVEL_NUMBER_MODE,
                HUD_ALIGNMENT,

                COORDINATE_FORMAT_STRING,
                DATE_FORMAT_REAL,
                DATE_FORMAT_MINECRAFT,

                BLOCK_GRID_OVERLAY_RADIUS,
                CHUNK_UNLOAD_BUCKET_FONT_SCALE,
                CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS,
                DROPPED_CHUNKS_HASH_SIZE,
                FONT_SCALE,
                LIGHT_LEVEL_MARKER_SIZE,
                LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X,
                LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y,
                LIGHT_LEVEL_NUMBER_OFFSET_SKY_X,
                LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y,
                LIGHT_LEVEL_RANGE,
                LIGHT_LEVEL_THRESHOLD,
                LIGHT_LEVEL_Z_OFFSET,
                MAP_PREVIEW_SIZE,
                SLIME_CHUNK_OVERLAY_RADIUS,
                SPAWNABLE_COLUMNS_OVERLAY_RADIUS,
                SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL,
                SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS,
                TEXT_POS_X,
                TEXT_POS_Y,
                TIME_DAY_DIVISOR,
                TIME_TOTAL_DIVISOR
        );

        public static final List<ConfigHotkey> HOTKEY_LIST = ImmutableList.of(
                TOGGLE_KEY,
                REQUIRED_KEY,
                OPEN_CONFIG_GUI,
                SET_DISTANCE_REFERENCE_POINT,
                SHAPE_EDITOR
        );
    }

    public static class Colors
    {
        public static final ConfigColor BLOCK_GRID_OVERLAY_COLOR            = new ConfigColor("blockGrid", "0x80FFFFFF", "Color for the block grid overlay");
        public static final ConfigColor DESPAWN_SPHERE_OVERLAY_COLOR        = new ConfigColor("despawnSphereDefault", "0x60A04050", "Default color for the despawn sphere overlay.\nThe color can be changed for each Despawn Sphere via its configuration GUI.");
        public static final ConfigColor LIGHT_LEVEL_MARKER_DARK             = new ConfigColor("lightLevelMarkerDark", "0xFFFF4848", "The color for the spawnable spots marker");
        public static final ConfigColor LIGHT_LEVEL_MARKER_LIT              = new ConfigColor("lightLevelMarkerLit", "0xFFFFFF33", "The color for the safe (during day) spots marker");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_BLOCK_DARK       = new ConfigColor("lightLevelNumberBlockDark", "0xFFC03030", "The color for the spawnable spots number of the block light value");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_BLOCK_LIT        = new ConfigColor("lightLevelNumberBlockLit", "0xFF209040", "The color for the safe spots number of the block light value");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_SKY_DARK         = new ConfigColor("lightLevelNumberSkyDark", "0xFFFFF030", "The color for the spawnable spots number of the sky light value");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_SKY_LIT          = new ConfigColor("lightLevelNumberSkyLit", "0xFF40E0FF", "The color for the safe spots number of the sky light value");
        public static final ConfigColor RANDOM_TICKS_FIXED_OVERLAY_COLOR    = new ConfigColor("randomTicksFixed", "0x40F9F225", "Color for the fixed-point random ticked chunks overlay");
        public static final ConfigColor RANDOM_TICKS_PLAYER_OVERLAY_COLOR   = new ConfigColor("randomTicksPlayer", "0x4030FE73", "Color for the player-following random ticked chunks overlay");
        public static final ConfigColor REGION_OVERLAY_COLOR                = new ConfigColor("regionFileBorders", "0x40FF8019", "Color for the region file overlay");
        public static final ConfigColor SLIME_CHUNKS_OVERLAY_COLOR          = new ConfigColor("slimeChunks", "0xB020F020", "Color for the slime chunks overlay");
        public static final ConfigColor SPAWN_PLAYER_ENTITY_OVERLAY_COLOR   = new ConfigColor("spawnPreviewAtPlayerEntity", "0x402050D0", "Color for the entity-processing spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_PLAYER_LAZY_OVERLAY_COLOR     = new ConfigColor("spawnPreviewAtPlayerLazy", "0x40D030D0", "Color for the \"lazy-loaded\" spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_REAL_ENTITY_OVERLAY_COLOR     = new ConfigColor("spawnChunksRealEntity", "0x4030FF20", "Color for the entity-processing real spawn chunks overlay");
        public static final ConfigColor SPAWN_REAL_LAZY_OVERLAY_COLOR       = new ConfigColor("spawnChunksRealLazy", "0x40FF3020", "Color for the \"lazy-loaded\" real spawn chunks overlay");
        public static final ConfigColor SPAWNABLE_CHUNKS_FIXED              = new ConfigColor("spawnableChunkFixed", "0x40FF2090", "Color for the location-fixed spawnable chunks overlay");
        public static final ConfigColor SPAWNABLE_CHUNKS_PLAYER             = new ConfigColor("spawnableChunksPlayer", "0x40FF3030", "Color for the player-following spawnable chunks overlay");
        public static final ConfigColor SPAWNABLE_COLUMNS_OVERLAY_COLOR     = new ConfigColor("spawnableColumnHeights", "0xA0FF00FF", "Color for the spawnable sub-chunks overlay");
        public static final ConfigColor TEXT_BACKGROUND_COLOR               = new ConfigColor("textBackgroundColor", "0xA0505050", "Info line HUD text background color");
        public static final ConfigColor TEXT_COLOR                          = new ConfigColor("textColor", "0xFFE0E0E0", "Info line text color");

        public static final ImmutableList<IConfigValue> OPTIONS = ImmutableList.of(
                BLOCK_GRID_OVERLAY_COLOR,
                DESPAWN_SPHERE_OVERLAY_COLOR,
                LIGHT_LEVEL_MARKER_DARK,
                LIGHT_LEVEL_MARKER_LIT,
                LIGHT_LEVEL_NUMBER_BLOCK_DARK,
                LIGHT_LEVEL_NUMBER_BLOCK_LIT,
                LIGHT_LEVEL_NUMBER_SKY_DARK,
                LIGHT_LEVEL_NUMBER_SKY_LIT,
                RANDOM_TICKS_FIXED_OVERLAY_COLOR,
                RANDOM_TICKS_PLAYER_OVERLAY_COLOR,
                REGION_OVERLAY_COLOR,
                SLIME_CHUNKS_OVERLAY_COLOR,
                SPAWN_PLAYER_ENTITY_OVERLAY_COLOR,
                SPAWN_PLAYER_LAZY_OVERLAY_COLOR,
                SPAWN_REAL_ENTITY_OVERLAY_COLOR,
                SPAWN_REAL_LAZY_OVERLAY_COLOR,
                SPAWNABLE_CHUNKS_FIXED,
                SPAWNABLE_CHUNKS_PLAYER,
                SPAWNABLE_COLUMNS_OVERLAY_COLOR,
                TEXT_BACKGROUND_COLOR,
                TEXT_COLOR
        );
    }

    @Override
    public String getModName()
    {
        return Reference.MOD_NAME;
    }

    @Override
    public String getConfigFileName()
    {
        return Reference.MOD_ID + ".json";
    }

    @Override
    public Map<String, List<? extends IConfigBase>> getConfigsPerCategories()
    {
        Map<String, List<? extends IConfigBase>> map = new LinkedHashMap<>();

        map.put("Generic", Generic.OPTIONS);
        map.put("Colors", Colors.OPTIONS);

        map.put("InfoTypeToggles", ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(InfoToggle.values())));
        map.put("InfoHotkeys", ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(InfoToggle.values())));
        map.put("InfoLineOrders",  ConfigUtils.createConfigWrapperForType(ConfigType.INTEGER, ImmutableList.copyOf(InfoToggle.values())));

        map.put("RendererToggles", ConfigUtils.createConfigWrapperForType(ConfigType.BOOLEAN, ImmutableList.copyOf(RendererToggle.values())));
        map.put("RendererHotkeys", ConfigUtils.createConfigWrapperForType(ConfigType.HOTKEY, ImmutableList.copyOf(RendererToggle.values())));

        map.put("StructureToggles", StructureToggle.getToggleConfigs());
        map.put("StructureHotkeys", StructureToggle.getHotkeys());
        map.put("StructureColors", StructureToggle.getColorConfigs());

        return map;
    }
}
