package fi.dy.masa.minihud.config;

import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.category.BaseConfigOptionCategory;
import fi.dy.masa.malilib.config.category.ConfigOptionCategory;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ColorConfig;
import fi.dy.masa.malilib.config.option.ConfigOption;
import fi.dy.masa.malilib.config.option.DoubleConfig;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.config.option.HotkeyedBooleanConfig;
import fi.dy.masa.malilib.config.option.IntegerConfig;
import fi.dy.masa.malilib.config.option.OptionListConfig;
import fi.dy.masa.malilib.config.option.StringConfig;
import fi.dy.masa.malilib.config.option.Vec2dConfig;
import fi.dy.masa.malilib.config.value.ScreenLocation;
import fi.dy.masa.malilib.input.Hotkey;
import fi.dy.masa.malilib.input.KeyBindSettings;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.util.value.BlockGridMode;
import fi.dy.masa.minihud.util.value.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.value.LightLevelNumberMode;

public class Configs
{
    public static final int CURRENT_VERSION = 1;

    public static class Generic
    {
        public static final HotkeyedBooleanConfig INFO_LINES_RENDERING_TOGGLE   = new HotkeyedBooleanConfig("infoLinesRendering", true, "H", KeyBindSettings.INGAME_RELEASE_EXCLUSIVE);
        public static final HotkeyedBooleanConfig OVERLAYS_RENDERING_TOGGLE     = new HotkeyedBooleanConfig("overlaysRendering", true, "");

        public static final BooleanConfig BEACON_RANGE_AUTO_UPDATE              = new BooleanConfig("beaconRangeAutoUpdate", false);
        public static final IntegerConfig BLOCK_GRID_OVERLAY_RADIUS             = new IntegerConfig("blockGridOverlayRadius", 16, 0, 64);
        public static final DoubleConfig  CHUNK_UNLOAD_BUCKET_FONT_SCALE        = new DoubleConfig( "chunkUnloadBucketOverlayFontScale", 0.1625, 0.0, 1.0);
        public static final BooleanConfig DEBUG_MESSAGES                        = new BooleanConfig("debugMessages", false);
        public static final IntegerConfig DROPPED_CHUNKS_HASH_SIZE              = new IntegerConfig("droppedChunksHashSize", -1, -1, Integer.MAX_VALUE);
        public static final IntegerConfig CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS    = new IntegerConfig("chunkUnloadBucketOverlayChunkRadius", -1, -1, 40);
        public static final BooleanConfig CHUNK_UNLOAD_BUCKET_HASH_SIZE         = new BooleanConfig("chunkUnloadBucketHashSize", true);
        public static final StringConfig  COORDINATE_FORMAT_STRING              = new StringConfig( "coordinateFormat", "x: %.1f y: %.1f z: %.1f");
        public static final BooleanConfig COORDINATE_FORMAT_CUSTOMIZED          = new BooleanConfig("coordinateFormatCustomized", false);
        public static final BooleanConfig PATH_FINDING_DEBUG_POINT_WIDTH        = new BooleanConfig("pathFindingDebugPointWidth", true);
        public static final BooleanConfig DONT_RESET_SEED_ON_DIMENSION_CHANGE   = new BooleanConfig("dontResetSeedOnDimensionChange", true);
        public static final BooleanConfig FIX_VANILLA_DEBUG_RENDERERS           = new BooleanConfig("enableVanillaDebugRendererFix", true);
        public static final BooleanConfig ITEM_NBT_ENABLED                      = new BooleanConfig("itemNbtEnabled", false);
        public static final IntegerConfig ITEM_PREVIEW_Z                        = new IntegerConfig("itemPreviewZ", 400, 0, 4096);
        public static final BooleanConfig LIGHT_LEVEL_COLORED_NUMBERS           = new BooleanConfig("lightLevelColoredNumbers", true);
        public static final DoubleConfig  LIGHT_LEVEL_MARKER_SIZE               = new DoubleConfig( "lightLevelMarkerSize", 0.84, 0.0, 1.0);
        public static final Vec2dConfig   LIGHT_LEVEL_NUMBER_OFFSET_BLOCK       = new Vec2dConfig(  "lightLevelNumberOffsetBlock", 0.24, 0.32, 0.0, 1.0);
        public static final Vec2dConfig   LIGHT_LEVEL_NUMBER_OFFSET_SKY         = new Vec2dConfig(  "lightLevelNumberOffsetSky", 0.42, 0.56, 0.0, 1.0);
        public static final BooleanConfig LIGHT_LEVEL_NUMBER_ROTATION           = new BooleanConfig("lightLevelNumberRotation", true);
        public static final IntegerConfig LIGHT_LEVEL_RANGE                     = new IntegerConfig("lightLevelRange", 24, 1, 64);
        public static final IntegerConfig LIGHT_LEVEL_THRESHOLD                 = new IntegerConfig("lightLevelThreshold", 8, 0, 15);
        public static final DoubleConfig  LIGHT_LEVEL_Z_OFFSET                  = new DoubleConfig( "lightLevelZOffset", 0.005, 0.0, 1.0);
        public static final BooleanConfig MAP_PREVIEW                           = new BooleanConfig("mapPreview", false);
        public static final BooleanConfig MAP_PREVIEW_REQUIRE_SHIFT             = new BooleanConfig("mapPreviewRequireShift", true);
        public static final IntegerConfig MAP_PREVIEW_SIZE                      = new IntegerConfig("mapPreviewSize", 160, 16, 512);
        public static final StringConfig  MC_TIME_FORMAT                        = new StringConfig( "mcTimeFormat", "MC time: (day {DAY}) {HOUR}:{MIN}:xx");
        public static final BooleanConfig OFFSET_SUBTITLE_HUD                   = new BooleanConfig("offsetSubtitleHud", false);
        public static final StringConfig  REAL_TIME_FORMAT                      = new StringConfig( "realTimeFormat", "yyyy-MM-dd HH:mm:ss");
        public static final BooleanConfig REQUIRE_SNEAK                         = new BooleanConfig("requireSneak", false);
        public static final BooleanConfig SHULKER_BOX_PREVIEW                   = new BooleanConfig("shulkerBoxPreview", false);
        public static final BooleanConfig SHULKER_DISPLAY_BACKGROUND_COLOR      = new BooleanConfig("shulkerDisplayBgColor", true);
        public static final BooleanConfig SHULKER_DISPLAY_REQUIRE_SHIFT         = new BooleanConfig("shulkerDisplayRequireShift", true);
        public static final IntegerConfig SLIME_CHUNK_OVERLAY_RADIUS            = new IntegerConfig("slimeChunkOverlayRadius", -1, -1, 64);
        public static final BooleanConfig SORT_LINES_BY_LENGTH                  = new BooleanConfig("sortLinesByLength", false);
        public static final BooleanConfig SORT_LINES_REVERSED                   = new BooleanConfig("sortLinesReversed", false);
        public static final IntegerConfig SPAWNABLE_COLUMNS_OVERLAY_RADIUS      = new IntegerConfig("spawnableColumnHeightsOverlayRadius", 40, 0, 128);
        public static final IntegerConfig SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL    = new IntegerConfig("spawnableSubChunkCheckInterval", 20, 1, 10000);
        public static final IntegerConfig SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS   = new IntegerConfig("spawnableSubChunksOverlayRadius", -1, -1, 40);
        public static final BooleanConfig SPAWNER_POSITION_PRINT                = new BooleanConfig("spawnerPositionPrint", false);
        public static final BooleanConfig STRUCTURES_RENDER_THROUGH             = new BooleanConfig("structuresRenderThrough", false);
        public static final IntegerConfig TIME_DAY_DIVISOR                      = new IntegerConfig("timeDayDivisor", 24000, 1, Integer.MAX_VALUE);
        public static final IntegerConfig TIME_TOTAL_DIVISOR                    = new IntegerConfig("timeTotalDivisor", 24000, 1, Integer.MAX_VALUE);
        public static final BooleanConfig WOOL_COUNTER_ENABLE_ALL               = new BooleanConfig("woolCounterEnableAll", true);
        public static final StringConfig  WOOL_COUNTER_TYPES                    = new StringConfig( "woolCounterTypes", "0-15");

        public static final OptionListConfig<BlockGridMode>         BLOCK_GRID_OVERLAY_MODE  = new OptionListConfig<>("blockGridOverlayMode", BlockGridMode.ALL, BlockGridMode.VALUES);
        public static final OptionListConfig<LightLevelMarkerMode>  LIGHT_LEVEL_MARKER_MODE  = new OptionListConfig<>("lightLevelMarkers", LightLevelMarkerMode.SQUARE, LightLevelMarkerMode.VALUES);
        public static final OptionListConfig<LightLevelNumberMode>  LIGHT_LEVEL_NUMBER_MODE  = new OptionListConfig<>("lightLevelNumbers", LightLevelNumberMode.BLOCK, LightLevelNumberMode.VALUES);

        public static final ImmutableList<ConfigOption<?>> OPTIONS = ImmutableList.of(
                BEACON_RANGE_AUTO_UPDATE,
                BLOCK_GRID_OVERLAY_MODE,
                BLOCK_GRID_OVERLAY_RADIUS,
                CHUNK_UNLOAD_BUCKET_FONT_SCALE,
                CHUNK_UNLOAD_BUCKET_HASH_SIZE,
                CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS,
                COORDINATE_FORMAT_STRING,
                COORDINATE_FORMAT_CUSTOMIZED,
                REAL_TIME_FORMAT,
                DEBUG_MESSAGES,
                PATH_FINDING_DEBUG_POINT_WIDTH,
                DONT_RESET_SEED_ON_DIMENSION_CHANGE,
                DROPPED_CHUNKS_HASH_SIZE,
                FIX_VANILLA_DEBUG_RENDERERS,
                INFO_LINES_RENDERING_TOGGLE,
                ITEM_NBT_ENABLED,
                ITEM_PREVIEW_Z,
                LIGHT_LEVEL_COLORED_NUMBERS,
                LIGHT_LEVEL_MARKER_MODE,
                LIGHT_LEVEL_MARKER_SIZE,
                LIGHT_LEVEL_NUMBER_MODE,
                LIGHT_LEVEL_NUMBER_OFFSET_BLOCK,
                LIGHT_LEVEL_NUMBER_OFFSET_SKY,
                LIGHT_LEVEL_NUMBER_ROTATION,
                LIGHT_LEVEL_RANGE,
                LIGHT_LEVEL_THRESHOLD,
                LIGHT_LEVEL_Z_OFFSET,
                MAP_PREVIEW,
                MAP_PREVIEW_REQUIRE_SHIFT,
                MAP_PREVIEW_SIZE,
                MC_TIME_FORMAT,
                OFFSET_SUBTITLE_HUD,
                OVERLAYS_RENDERING_TOGGLE,
                REQUIRE_SNEAK,
                SHULKER_BOX_PREVIEW,
                SHULKER_DISPLAY_BACKGROUND_COLOR,
                SHULKER_DISPLAY_REQUIRE_SHIFT,
                SLIME_CHUNK_OVERLAY_RADIUS,
                SORT_LINES_BY_LENGTH,
                SORT_LINES_REVERSED,
                SPAWNABLE_COLUMNS_OVERLAY_RADIUS,
                SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL,
                SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS,
                SPAWNER_POSITION_PRINT,
                STRUCTURES_RENDER_THROUGH,
                TIME_DAY_DIVISOR,
                TIME_TOTAL_DIVISOR,
                WOOL_COUNTER_ENABLE_ALL,
                WOOL_COUNTER_TYPES
        );

        public static final List<Hotkey> HOTKEY_LIST = ImmutableList.of(
                INFO_LINES_RENDERING_TOGGLE,
                OVERLAYS_RENDERING_TOGGLE
        );
    }

    public static class Colors
    {
        public static final ColorConfig BEACON_RANGE_LVL1_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl1",            "#20E060FF");
        public static final ColorConfig BEACON_RANGE_LVL2_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl2",            "#20FFB040");
        public static final ColorConfig BEACON_RANGE_LVL3_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl3",            "#20FFF040");
        public static final ColorConfig BEACON_RANGE_LVL4_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl4",            "#2060FF40");
        public static final ColorConfig BLOCK_GRID_OVERLAY_COLOR            = new ColorConfig("blockGrid",                  "#80FFFFFF");
        public static final ColorConfig LIGHT_LEVEL_MARKER_DARK             = new ColorConfig("lightLevelMarkerDark",       "#FFFF4848");
        public static final ColorConfig LIGHT_LEVEL_MARKER_LIT              = new ColorConfig("lightLevelMarkerLit",        "#FFFFFF33");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_BLOCK_DARK       = new ColorConfig("lightLevelNumberBlockDark",  "#FFC03030");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_BLOCK_LIT        = new ColorConfig("lightLevelNumberBlockLit",   "#FF209040");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_SKY_DARK         = new ColorConfig("lightLevelNumberSkyDark",    "#FFFFF030");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_SKY_LIT          = new ColorConfig("lightLevelNumberSkyLit",     "#FF40E0FF");
        public static final ColorConfig RANDOM_TICKS_FIXED_OVERLAY_COLOR    = new ColorConfig("randomTicksFixed",           "#40F9F225");
        public static final ColorConfig RANDOM_TICKS_PLAYER_OVERLAY_COLOR   = new ColorConfig("randomTicksPlayer",          "#4030FE73");
        public static final ColorConfig REGION_OVERLAY_COLOR                = new ColorConfig("regionFileBorders",          "#40FF8019");
        public static final ColorConfig SHAPE_CAN_DESPAWN_SPHERE            = new ColorConfig("shapeCanDespawnSphere",      "#60A04050");
        public static final ColorConfig SHAPE_CAN_SPAWN_SPHERE              = new ColorConfig("shapeCanSpawnSphere",        "#60A04050");
        public static final ColorConfig SHAPE_CIRCLE                        = new ColorConfig("shapeCircle",                "#6030B0B0");
        public static final ColorConfig SHAPE_DESPAWN_SPHERE                = new ColorConfig("shapeDespawnSphere",         "#60A04050");
        public static final ColorConfig SHAPE_SPHERE_BLOCKY                 = new ColorConfig("shapeSphereBlocky",          "#6030B0B0");
        public static final ColorConfig SLIME_CHUNKS_OVERLAY_COLOR          = new ColorConfig("slimeChunks",                "#B020F020");
        public static final ColorConfig SPAWN_PLAYER_ENTITY_OVERLAY_COLOR   = new ColorConfig("spawnPreviewAtPlayerEntity", "#402050D0");
        public static final ColorConfig SPAWN_PLAYER_LAZY_OVERLAY_COLOR     = new ColorConfig("spawnPreviewAtPlayerLazy",   "#40D030D0");
        public static final ColorConfig SPAWN_REAL_ENTITY_OVERLAY_COLOR     = new ColorConfig("spawnChunksRealEntity",      "#4030FF20");
        public static final ColorConfig SPAWN_REAL_LAZY_OVERLAY_COLOR       = new ColorConfig("spawnChunksRealLazy",        "#40FF3020");
        public static final ColorConfig SPAWNABLE_CHUNKS_FIXED              = new ColorConfig("spawnableChunkFixed",        "#40FF2090");
        public static final ColorConfig SPAWNABLE_CHUNKS_PLAYER             = new ColorConfig("spawnableChunksPlayer",      "#40FF3030");
        public static final ColorConfig SPAWNABLE_COLUMNS_OVERLAY_COLOR     = new ColorConfig("spawnableColumnHeights",     "#A0FF00FF");
        public static final ColorConfig SPAWNER_POSITIONS_OVERLAY_COLOR     = new ColorConfig("spawnerPositions",           "#40FF8019");
        public static final ColorConfig WATER_FALL_POSITIONS_OVERLAY_COLOR  = new ColorConfig("waterFallPositions",         "#40326BF3");

        public static final ImmutableList<ConfigOption<?>> OPTIONS = ImmutableList.of(
                BEACON_RANGE_LVL1_OVERLAY_COLOR,
                BEACON_RANGE_LVL2_OVERLAY_COLOR,
                BEACON_RANGE_LVL3_OVERLAY_COLOR,
                BEACON_RANGE_LVL4_OVERLAY_COLOR,
                BLOCK_GRID_OVERLAY_COLOR,
                LIGHT_LEVEL_MARKER_DARK,
                LIGHT_LEVEL_MARKER_LIT,
                LIGHT_LEVEL_NUMBER_BLOCK_DARK,
                LIGHT_LEVEL_NUMBER_BLOCK_LIT,
                LIGHT_LEVEL_NUMBER_SKY_DARK,
                LIGHT_LEVEL_NUMBER_SKY_LIT,
                RANDOM_TICKS_FIXED_OVERLAY_COLOR,
                RANDOM_TICKS_PLAYER_OVERLAY_COLOR,
                REGION_OVERLAY_COLOR,
                SHAPE_CAN_DESPAWN_SPHERE,
                SHAPE_CAN_SPAWN_SPHERE,
                SHAPE_CIRCLE,
                SHAPE_DESPAWN_SPHERE,
                SHAPE_SPHERE_BLOCKY,
                SLIME_CHUNKS_OVERLAY_COLOR,
                SPAWN_PLAYER_ENTITY_OVERLAY_COLOR,
                SPAWN_PLAYER_LAZY_OVERLAY_COLOR,
                SPAWN_REAL_ENTITY_OVERLAY_COLOR,
                SPAWN_REAL_LAZY_OVERLAY_COLOR,
                SPAWNABLE_CHUNKS_FIXED,
                SPAWNABLE_CHUNKS_PLAYER,
                SPAWNABLE_COLUMNS_OVERLAY_COLOR,
                SPAWNER_POSITIONS_OVERLAY_COLOR,
                WATER_FALL_POSITIONS_OVERLAY_COLOR
        );
    }

    public static class Hotkeys
    {
        public static final HotkeyConfig  ITEM_NBT_KEY_PRETTY           = new HotkeyConfig("itemNbtKeyPretty",          "", KeyBindSettings.GUI_MODIFIER);
        public static final HotkeyConfig  ITEM_NBT_KEY_STRING           = new HotkeyConfig("itemNbtKeyString",          "", KeyBindSettings.GUI_MODIFIER);
        public static final HotkeyConfig  OPEN_CONFIG_GUI               = new HotkeyConfig("openConfigGui",             "H,C");
        public static final HotkeyConfig  OPEN_SHAPE_EDITOR             = new HotkeyConfig("openShapeEditor",           "");
        public static final HotkeyConfig  OPEN_SHAPE_MANAGER            = new HotkeyConfig("openShapeManager",          "");
        public static final HotkeyConfig  REQUIRED_KEY                  = new HotkeyConfig("requiredKey",               "", KeyBindSettings.INGAME_MODIFIER_EMPTY);
        public static final HotkeyConfig  SET_DISTANCE_REFERENCE_POINT  = new HotkeyConfig("setDistanceReferencePoint", "");

        public static final List<HotkeyConfig> HOTKEYS = ImmutableList.of(
                ITEM_NBT_KEY_PRETTY,
                ITEM_NBT_KEY_STRING,
                OPEN_CONFIG_GUI,
                OPEN_SHAPE_EDITOR,
                REQUIRED_KEY,
                SET_DISTANCE_REFERENCE_POINT
        );
    }

    public static class Internal
    {
        public static final OptionListConfig<ScreenLocation> HUD_LOCATION   = new OptionListConfig<>("infoLinesHudLocation", ScreenLocation.TOP_LEFT, ScreenLocation.VALUES);
        public static final DoubleConfig CHUNK_UNLOAD_BUCKET_OVERLAY_Y      = new DoubleConfig("chunkUnloadBucketOverlayY", 80);
        public static final DoubleConfig SLIME_CHUNKS_OVERLAY_TOP_Y         = new DoubleConfig("slimeChunksOverlayTopY", 80);

        public static final ImmutableList<ConfigOption<?>> OPTIONS = ImmutableList.of(
                CHUNK_UNLOAD_BUCKET_OVERLAY_Y,
                HUD_LOCATION,
                SLIME_CHUNKS_OVERLAY_TOP_Y
        );
    }

    public static final List<ConfigOptionCategory> CATEGORIES = ImmutableList.of(
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "Generic",          Generic.OPTIONS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "Hotkeys",          Hotkeys.HOTKEYS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "Colors",           Colors.OPTIONS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "InfoToggles",      InfoLineToggle.TOGGLE_CONFIGS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "InfoHotkeys",      InfoLineToggle.TOGGLE_HOTKEYS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "InfoLineOrders",   InfoLineToggle.LINE_ORDER_CONFIGS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "Internal",         Internal.OPTIONS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "RendererToggles",  RendererToggle.TOGGLE_CONFIGS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "RendererHotkeys",  RendererToggle.TOGGLE_HOTKEYS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "StructureToggles", StructureToggle.TOGGLE_CONFIGS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "StructureHotkeys", StructureToggle.TOGGLE_HOTKEYS),
            BaseConfigOptionCategory.normal(Reference.MOD_INFO, "StructureColors",  StructureToggle.COLOR_CONFIGS)
    );
}
