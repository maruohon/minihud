package fi.dy.masa.minihud.config;

import java.util.List;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.option.ConfigOption;
import fi.dy.masa.malilib.config.category.BaseConfigOptionCategory;
import fi.dy.masa.malilib.config.category.ConfigOptionCategory;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ColorConfig;
import fi.dy.masa.malilib.config.option.DoubleConfig;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.config.option.HotkeyedBooleanConfig;
import fi.dy.masa.malilib.config.option.IntegerConfig;
import fi.dy.masa.malilib.config.option.OptionListConfig;
import fi.dy.masa.malilib.config.option.StringConfig;
import fi.dy.masa.malilib.config.value.HudAlignment;
import fi.dy.masa.malilib.input.Hotkey;
import fi.dy.masa.malilib.input.KeyBindSettings;
import fi.dy.masa.minihud.util.BlockGridMode;
import fi.dy.masa.minihud.util.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.LightLevelNumberMode;
import fi.dy.masa.minihud.util.PrintMode;

public class Configs
{
    public static class Generic
    {
        public static final BooleanConfig BEACON_RANGE_AUTO_UPDATE              = new BooleanConfig("beaconRangeAutoUpdate", false);
        public static final IntegerConfig BLOCK_GRID_OVERLAY_RADIUS             = new IntegerConfig("blockGridOverlayRadius", 32);
        public static final DoubleConfig CHUNK_UNLOAD_BUCKET_FONT_SCALE         = new DoubleConfig("chunkUnloadBucketOverlayFontScale", 0.1625, 0.0, 1.0);
        public static final BooleanConfig DEBUG_MESSAGES                        = new BooleanConfig("debugMessages", false);
        public static final IntegerConfig DROPPED_CHUNKS_HASH_SIZE              = new IntegerConfig("droppedChunksHashSize", -1, -1, Integer.MAX_VALUE);
        public static final IntegerConfig CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS    = new IntegerConfig("chunkUnloadBucketOverlayChunkRadius", -1, -1, 40);
        public static final BooleanConfig CHUNK_UNLOAD_BUCKET_HASH_SIZE         = new BooleanConfig("chunkUnloadBucketHashSize", true);
        public static final StringConfig COORDINATE_FORMAT_STRING               = new StringConfig("coordinateFormat", "x: %.1f y: %.1f z: %.1f");
        public static final BooleanConfig COORDINATE_FORMAT_CUSTOMIZED          = new BooleanConfig("coordinateFormatCustomized", false);
        public static final BooleanConfig DEBUG_RENDERER_PATH_MAX_DIST          = new BooleanConfig("debugRendererPathFindingEnablePointWidth", true);
        public static final BooleanConfig DONT_RESET_SEED_ON_DIMENSION_CHANGE   = new BooleanConfig("dontResetSeedOnDimensionChange", false);
        public static final BooleanConfig FIX_VANILLA_DEBUG_RENDERERS           = new BooleanConfig("enableVanillaDebugRendererFix", true);
        public static final DoubleConfig HUD_FONT_SCALE                         = new DoubleConfig("hudFontScale", 0.5, 0.0, 100.0);
        public static final IntegerConfig HUD_TEXT_POS_X                        = new IntegerConfig("hudTextPosX", 4, -4096, 4096);
        public static final IntegerConfig HUD_TEXT_POS_Y                        = new IntegerConfig("hudTextPosY", 4, -4096, 4096);
        public static final BooleanConfig ITEM_NBT_ENABLED                      = new BooleanConfig("itemNbtEnabled", false);
        public static final HotkeyConfig ITEM_NBT_KEY_PRETTY                    = new HotkeyConfig("itemNbtKeyPretty", "", KeyBindSettings.MODIFIER_GUI);
        public static final HotkeyConfig ITEM_NBT_KEY_STRING                    = new HotkeyConfig("itemNbtKeyString", "", KeyBindSettings.MODIFIER_GUI);
        public static final BooleanConfig LIGHT_LEVEL_COLORED_NUMBERS           = new BooleanConfig("lightLevelColoredNumbers", true);
        public static final DoubleConfig LIGHT_LEVEL_MARKER_SIZE                = new DoubleConfig("lightLevelMarkerSize", 0.84, 0.0, 1.0);
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X      = new DoubleConfig("lightLevelNumberOffsetBlockX", 0.09, 0.0, 1.0);
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y      = new DoubleConfig("lightLevelNumberOffsetBlockY", 0.12, 0.0, 1.0);
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_SKY_X        = new DoubleConfig("lightLevelNumberOffsetSkyX", 0.42, 0.0, 1.0);
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y        = new DoubleConfig("lightLevelNumberOffsetSkyY", 0.56, 0.0, 1.0);
        public static final BooleanConfig LIGHT_LEVEL_NUMBER_ROTATION           = new BooleanConfig("lightLevelNumberRotation", true);
        public static final IntegerConfig LIGHT_LEVEL_RANGE                     = new IntegerConfig("lightLevelRange", 24, 1, 64);
        public static final IntegerConfig LIGHT_LEVEL_THRESHOLD                 = new IntegerConfig("lightLevelThreshold", 8, 0, 15);
        public static final DoubleConfig LIGHT_LEVEL_Z_OFFSET                   = new DoubleConfig("lightLevelZOffset", 0.005, 0.0, 1.0);
        public static final HotkeyedBooleanConfig MAIN_RENDERING_TOGGLE         = new HotkeyedBooleanConfig("mainRenderingToggle", true, "", KeyBindSettings.RELEASE_EXCLUSIVE);
        public static final BooleanConfig MAP_PREVIEW                           = new BooleanConfig("mapPreview", false);
        public static final IntegerConfig MAP_PREVIEW_SIZE                      = new IntegerConfig("mapPreviewSize", 160, 16, 512);
        public static final StringConfig MC_TIME_FORMAT                         = new StringConfig("mcTimeFormat", "MC time: (day {DAY}) {HOUR}:{MIN}:xx");
        public static final HotkeyConfig OPEN_CONFIG_GUI                        = new HotkeyConfig("openConfigGui", "H,C");
        public static final StringConfig REAL_TIME_FORMAT                       = new StringConfig("realTimeFormat", "yyyy-MM-dd HH:mm:ss");
        public static final BooleanConfig REQUIRE_SNEAK                         = new BooleanConfig("requireSneak", false);
        public static final HotkeyConfig REQUIRED_KEY                           = new HotkeyConfig("requiredKey", "", KeyBindSettings.MODIFIER_INGAME_EMPTY);
        public static final HotkeyConfig SET_DISTANCE_REFERENCE_POINT           = new HotkeyConfig("setDistanceReferencePoint", "");
        public static final HotkeyConfig SHAPE_EDITOR                           = new HotkeyConfig("shapeEditor", "");
        public static final BooleanConfig SHULKER_BOX_PREVIEW                   = new BooleanConfig("shulkerBoxPreview", false);
        public static final BooleanConfig SHULKER_DISPLAY_BACKGROUND_COLOR      = new BooleanConfig("shulkerDisplayBgColor", true);
        public static final BooleanConfig SHULKER_DISPLAY_REQUIRE_SHIFT         = new BooleanConfig("shulkerDisplayRequireShift", true);
        public static final IntegerConfig SLIME_CHUNK_OVERLAY_RADIUS            = new IntegerConfig("slimeChunkOverlayRadius", -1, -1, 64);
        public static final BooleanConfig SORT_LINES_BY_LENGTH                  = new BooleanConfig("sortLinesByLength", false);
        public static final BooleanConfig SORT_LINES_REVERSED                   = new BooleanConfig("sortLinesReversed", false);
        public static final IntegerConfig SPAWNABLE_COLUMNS_OVERLAY_RADIUS      = new IntegerConfig("spawnableColumnHeightsOverlayRadius", 40, 0, 128);
        public static final IntegerConfig SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL    = new IntegerConfig("spawnableSubChunkCheckInterval", 20, 1, 10000);
        public static final IntegerConfig SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS   = new IntegerConfig("spawnableSubChunksOverlayRadius", -1, -1, 40);
        public static final BooleanConfig STRUCTURES_RENDER_THROUGH             = new BooleanConfig("structuresRenderThrough", false);
        public static final IntegerConfig TIME_DAY_DIVISOR                      = new IntegerConfig("timeDayDivisor", 24000, 1, Integer.MAX_VALUE);
        public static final IntegerConfig TIME_TOTAL_DIVISOR                    = new IntegerConfig("timeTotalDivisor", 24000, 1, Integer.MAX_VALUE);
        public static final BooleanConfig USE_FONT_SHADOW                       = new BooleanConfig("useFontShadow", false);
        public static final BooleanConfig USE_TEXT_BACKGROUND                   = new BooleanConfig("useTextBackground", true);
        public static final BooleanConfig WOOL_COUNTER_ENABLE_ALL               = new BooleanConfig("woolCounterEnableAll", true);
        public static final StringConfig WOOL_COUNTER_TYPES                     = new StringConfig("woolCounterTypes", "0-15");

        public static final OptionListConfig<BlockGridMode> BLOCK_GRID_OVERLAY_MODE         = new OptionListConfig<>("blockGridOverlayMode", BlockGridMode.ALL, BlockGridMode.VALUES);
        public static final OptionListConfig<HudAlignment> HUD_ALIGNMENT                    = new OptionListConfig<>("infoLinesHudAlignment", HudAlignment.TOP_LEFT, HudAlignment.VALUES);
        public static final OptionListConfig<LightLevelMarkerMode> LIGHT_LEVEL_MARKER_MODE  = new OptionListConfig<>("lightLevelMarkers", LightLevelMarkerMode.SQUARE, LightLevelMarkerMode.VALUES);
        public static final OptionListConfig<LightLevelNumberMode> LIGHT_LEVEL_NUMBER_MODE  = new OptionListConfig<>("lightLevelNumbers", LightLevelNumberMode.BLOCK, LightLevelNumberMode.VALUES);
        public static final OptionListConfig<PrintMode> SPAWNER_POSITION_PRINT              = new OptionListConfig<>("spawnerPositionPrint", PrintMode.SUCCESS, PrintMode.VALUES);

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
                DEBUG_RENDERER_PATH_MAX_DIST,
                DONT_RESET_SEED_ON_DIMENSION_CHANGE,
                DROPPED_CHUNKS_HASH_SIZE,
                FIX_VANILLA_DEBUG_RENDERERS,
                HUD_ALIGNMENT,
                HUD_FONT_SCALE,
                HUD_TEXT_POS_X,
                HUD_TEXT_POS_Y,
                ITEM_NBT_ENABLED,
                ITEM_NBT_KEY_PRETTY,
                ITEM_NBT_KEY_STRING,
                LIGHT_LEVEL_COLORED_NUMBERS,
                LIGHT_LEVEL_MARKER_MODE,
                LIGHT_LEVEL_MARKER_SIZE,
                LIGHT_LEVEL_NUMBER_MODE,
                LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X,
                LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y,
                LIGHT_LEVEL_NUMBER_OFFSET_SKY_X,
                LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y,
                LIGHT_LEVEL_NUMBER_ROTATION,
                LIGHT_LEVEL_RANGE,
                LIGHT_LEVEL_THRESHOLD,
                LIGHT_LEVEL_Z_OFFSET,
                MAIN_RENDERING_TOGGLE,
                MAP_PREVIEW,
                MAP_PREVIEW_SIZE,
                MC_TIME_FORMAT,
                OPEN_CONFIG_GUI,
                REQUIRE_SNEAK,
                REQUIRED_KEY,
                SET_DISTANCE_REFERENCE_POINT,
                SHAPE_EDITOR,
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
                USE_FONT_SHADOW,
                USE_TEXT_BACKGROUND,
                WOOL_COUNTER_ENABLE_ALL,
                WOOL_COUNTER_TYPES
        );

        public static final List<Hotkey> HOTKEY_LIST = ImmutableList.of(
                ITEM_NBT_KEY_PRETTY,
                ITEM_NBT_KEY_STRING,
                MAIN_RENDERING_TOGGLE,
                REQUIRED_KEY,
                OPEN_CONFIG_GUI,
                SET_DISTANCE_REFERENCE_POINT,
                SHAPE_EDITOR
        );
    }

    public static class Colors
    {
        public static final ColorConfig BEACON_RANGE_LVL1_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl1", "0x20E060FF");
        public static final ColorConfig BEACON_RANGE_LVL2_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl2", "0x20FFB040");
        public static final ColorConfig BEACON_RANGE_LVL3_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl3", "0x20FFF040");
        public static final ColorConfig BEACON_RANGE_LVL4_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl4", "0x2060FF40");
        public static final ColorConfig BLOCK_GRID_OVERLAY_COLOR            = new ColorConfig("blockGrid", "0x80FFFFFF");
        public static final ColorConfig HUD_TEXT_BACKGROUND                 = new ColorConfig("hudTextBackground", "0xA0505050");
        public static final ColorConfig HUD_TEXT                            = new ColorConfig("hudText", "0xFFE0E0E0");
        public static final ColorConfig LIGHT_LEVEL_MARKER_DARK             = new ColorConfig("lightLevelMarkerDark", "0xFFFF4848");
        public static final ColorConfig LIGHT_LEVEL_MARKER_LIT              = new ColorConfig("lightLevelMarkerLit", "0xFFFFFF33");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_BLOCK_DARK       = new ColorConfig("lightLevelNumberBlockDark", "0xFFC03030");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_BLOCK_LIT        = new ColorConfig("lightLevelNumberBlockLit", "0xFF209040");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_SKY_DARK         = new ColorConfig("lightLevelNumberSkyDark", "0xFFFFF030");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_SKY_LIT          = new ColorConfig("lightLevelNumberSkyLit", "0xFF40E0FF");
        public static final ColorConfig RANDOM_TICKS_FIXED_OVERLAY_COLOR    = new ColorConfig("randomTicksFixed", "0x40F9F225");
        public static final ColorConfig RANDOM_TICKS_PLAYER_OVERLAY_COLOR   = new ColorConfig("randomTicksPlayer", "0x4030FE73");
        public static final ColorConfig REGION_OVERLAY_COLOR                = new ColorConfig("regionFileBorders", "0x40FF8019");
        public static final ColorConfig SHAPE_CAN_DESPAWN_SPHERE            = new ColorConfig("shapeCanDespawnSphere", "0x60A04050");
        public static final ColorConfig SHAPE_CAN_SPAWN_SPHERE              = new ColorConfig("shapeCanSpawnSphere", "0x60A04050");
        public static final ColorConfig SHAPE_CIRCLE                        = new ColorConfig("shapeCircle", "0x6030B0B0");
        public static final ColorConfig SHAPE_DESPAWN_SPHERE                = new ColorConfig("shapeDespawnSphere", "0x60A04050");
        public static final ColorConfig SHAPE_SPHERE_BLOCKY                 = new ColorConfig("shapeSphereBlocky", "0x6030B0B0");
        public static final ColorConfig SLIME_CHUNKS_OVERLAY_COLOR          = new ColorConfig("slimeChunks", "0xB020F020");
        public static final ColorConfig SPAWN_PLAYER_ENTITY_OVERLAY_COLOR   = new ColorConfig("spawnPreviewAtPlayerEntity", "0x402050D0");
        public static final ColorConfig SPAWN_PLAYER_LAZY_OVERLAY_COLOR     = new ColorConfig("spawnPreviewAtPlayerLazy", "0x40D030D0");
        public static final ColorConfig SPAWN_REAL_ENTITY_OVERLAY_COLOR     = new ColorConfig("spawnChunksRealEntity", "0x4030FF20");
        public static final ColorConfig SPAWN_REAL_LAZY_OVERLAY_COLOR       = new ColorConfig("spawnChunksRealLazy", "0x40FF3020");
        public static final ColorConfig SPAWNABLE_CHUNKS_FIXED              = new ColorConfig("spawnableChunkFixed", "0x40FF2090");
        public static final ColorConfig SPAWNABLE_CHUNKS_PLAYER             = new ColorConfig("spawnableChunksPlayer", "0x40FF3030");
        public static final ColorConfig SPAWNABLE_COLUMNS_OVERLAY_COLOR     = new ColorConfig("spawnableColumnHeights", "0xA0FF00FF");
        public static final ColorConfig SPAWNER_POSITIONS_OVERLAY_COLOR     = new ColorConfig("spawnerPositions", "#40FF8019");

        public static final ImmutableList<ConfigOption<?>> OPTIONS = ImmutableList.of(
                BEACON_RANGE_LVL1_OVERLAY_COLOR,
                BEACON_RANGE_LVL2_OVERLAY_COLOR,
                BEACON_RANGE_LVL3_OVERLAY_COLOR,
                BEACON_RANGE_LVL4_OVERLAY_COLOR,
                BLOCK_GRID_OVERLAY_COLOR,
                HUD_TEXT,
                HUD_TEXT_BACKGROUND,
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
                SPAWNER_POSITIONS_OVERLAY_COLOR
        );
    }

    public static final List<ConfigOptionCategory> CATEGORIES = ImmutableList.of(
            BaseConfigOptionCategory.normal("Generic",          Configs.Generic.OPTIONS),
            BaseConfigOptionCategory.normal("Colors",           Configs.Colors.OPTIONS),
            BaseConfigOptionCategory.normal("InfoTypeToggles",  InfoLine.TOGGLE_CONFIGS),
            BaseConfigOptionCategory.normal("InfoLineOrders",   InfoLine.LINE_ORDER_CONFIGS),
            BaseConfigOptionCategory.normal("InfoHotkeys",      InfoLine.TOGGLE_HOTKEYS),
            BaseConfigOptionCategory.normal("RendererToggles",  RendererToggle.TOGGLE_CONFIGS),
            BaseConfigOptionCategory.normal("RendererHotkeys",  RendererToggle.TOGGLE_HOTKEYS),
            BaseConfigOptionCategory.normal("StructureToggles", StructureToggle.TOGGLE_CONFIGS),
            BaseConfigOptionCategory.normal("StructureHotkeys", StructureToggle.TOGGLE_HOTKEYS),
            BaseConfigOptionCategory.normal("StructureColors",  StructureToggle.COLOR_CONFIGS)
    );
}
