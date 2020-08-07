package fi.dy.masa.minihud.config;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.ConfigUtils;
import fi.dy.masa.malilib.config.ModConfig;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ColorConfig;
import fi.dy.masa.malilib.config.option.ConfigOption;
import fi.dy.masa.malilib.config.option.DoubleConfig;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.config.option.IntegerConfig;
import fi.dy.masa.malilib.config.option.OptionListConfig;
import fi.dy.masa.malilib.config.option.StringConfig;
import fi.dy.masa.malilib.config.value.HudAlignment;
import fi.dy.masa.malilib.input.KeyBindSettings;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.util.BlockGridMode;
import fi.dy.masa.minihud.util.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.LightLevelNumberMode;

public class Configs implements ModConfig
{
    public static class Generic
    {
        public static final BooleanConfig BEACON_RANGE_AUTO_UPDATE              = new BooleanConfig("beaconRangeAutoUpdate", false, "This enables the Beacon Range renderer to update automatically\nwhen block or chunk change packets are received.\nThose updates can have a performance impact if tons of blocks are changing often.\nYou can toggle off/on the renderer to update to those changes manually.");
        public static final IntegerConfig BLOCK_GRID_OVERLAY_RADIUS             = new IntegerConfig("blockGridOverlayRadius", 32, "The radius of the block grid lines to render");
        public static final DoubleConfig CHUNK_UNLOAD_BUCKET_FONT_SCALE         = new DoubleConfig("chunkUnloadBucketOverlayFontScale", 0.1625, "The font scale for the Chunk unload order bucket overlay.\nValid range: 0.01 - 1.0");
        public static final BooleanConfig DEBUG_MESSAGES                        = new BooleanConfig("debugMessages", false, "Enables some debug messages in the game console");
        public static final IntegerConfig DROPPED_CHUNKS_HASH_SIZE              = new IntegerConfig("droppedChunksHashSize", -1, -1, Integer.MAX_VALUE, "The HashSet size for the chunk unload bucket calculation,\nif 'chunkUnloadBucketWithSize' is true.\nUse -1 for automatically getting the current value in single player,\nor on Carpet servers. A value other than -1 will override the automatic value,\nincluding the proper value received from a Carpet server.");
        public static final IntegerConfig CHUNK_UNLOAD_BUCKET_OVERLAY_RADIUS    = new IntegerConfig("chunkUnloadBucketOverlayChunkRadius", -1, "The radius of chunks to render the text for in the overlay.\nValid range: -1 - 40, where -1 = render distance");
        public static final BooleanConfig CHUNK_UNLOAD_BUCKET_HASH_SIZE         = new BooleanConfig("chunkUnloadBucketHashSize", true, "If enabled, uses the more accurate (but still experimental)\nchunk unload bucket calculations, taken from the Carpet mod");
        public static final StringConfig COORDINATE_FORMAT_STRING               = new StringConfig("coordinateFormat", "x: %.1f y: %.1f z: %.1f", "The format string for the coordinate line ('infoCoordinates').\nNeeds to have three %f format strings!\nDefault: x: %.1f y: %.1f z: %.1f");
        public static final StringConfig DATE_FORMAT_REAL                       = new StringConfig("dateFormatReal", "yyyy-MM-dd HH:mm:ss", "The format string for real time, see the Java SimpleDateFormat\nclass for the format patterns, if needed.");
        public static final StringConfig DATE_FORMAT_MINECRAFT                  = new StringConfig("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx", "The format string for the Minecraft time.\nThe supported placeholders are: {DAY_1}, {DAY}, {HOUR}, {MIN}, {SEC}.\n{DAY_1} starts the day counter from 1, {DAY} starts from 0.");
        public static final BooleanConfig DEBUG_RENDERER_PATH_MAX_DIST          = new BooleanConfig("debugRendererPathFindingEnablePointWidth", true, "If true, then the vanilla pathfinding debug renderer\nwill render the path point width boxes.");
        public static final BooleanConfig DONT_RESET_SEED_ON_DIMENSION_CHANGE   = new BooleanConfig("dontResetSeedOnDimensionChange", false, "Don't reset the world seed when just changing dimensions.\nSome mods may use per-dimension seeds, so by default the seed\nis reset every time the player changes dimensions.");
        public static final BooleanConfig ENABLED                               = new BooleanConfig("enabled", true, "The main rendering toggle for all MiniHUD rendering");
        public static final BooleanConfig FIX_VANILLA_DEBUG_RENDERERS           = new BooleanConfig("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed.");
        public static final DoubleConfig FONT_SCALE                             = new DoubleConfig("fontScale", 0.5, 0.0, 100.0, "Font scale factor for the info line HUD. Default: 0.5\n");
        public static final BooleanConfig ITEM_NBT_ENABLED                      = new BooleanConfig("itemNbtEnabled", false, "Enables showing the item NBT data in the tooltip,\nif one of the activation keys is held.");
        public static final HotkeyConfig ITEM_NBT_KEY_PRETTY                    = new HotkeyConfig("itemNbtKeyPretty", "", KeyBindSettings.MODIFIER_GUI, "Shows pretty formatted item NBT data");
        public static final HotkeyConfig ITEM_NBT_KEY_STRING                    = new HotkeyConfig("itemNbtKeyString", "", KeyBindSettings.MODIFIER_GUI, "Shows item NBT data in the stringified format");
        public static final BooleanConfig LIGHT_LEVEL_COLORED_NUMBERS           = new BooleanConfig("lightLevelColoredNumbers", true, "Whether to use colored or white numbers\nfor the Light Level overlay numbers");
        public static final DoubleConfig LIGHT_LEVEL_MARKER_SIZE                = new DoubleConfig("lightLevelMarkerSize", 0.84, 0.0, 1.0, "The size of the light level colored marker.\nRange: 0.0 - 1.0");
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X      = new DoubleConfig("lightLevelNumberOffsetBlockX", 0.09, 0.0, 1.0, "The relative \"x\" offset for the block light level number.\nRange: 0.0 - 1.0");
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y      = new DoubleConfig("lightLevelNumberOffsetBlockY", 0.12, 0.0, 1.0, "The relative \"y\" offset for the block light level number.\nRange: 0.0 - 1.0");
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_SKY_X        = new DoubleConfig("lightLevelNumberOffsetSkyX", 0.42, 0.0, 1.0, "The relative \"x\" offset for the sky light level number.\nRange: 0.0 - 1.0");
        public static final DoubleConfig LIGHT_LEVEL_NUMBER_OFFSET_SKY_Y        = new DoubleConfig("lightLevelNumberOffsetSkyY", 0.56, 0.0, 1.0, "The relative \"y\" offset for the sky light level number.\nRange: 0.0 - 1.0");
        public static final BooleanConfig LIGHT_LEVEL_NUMBER_ROTATION           = new BooleanConfig("lightLevelNumberRotation", true, "If true, then the light level numbers will rotate\naccording to the player's current facing");
        public static final IntegerConfig LIGHT_LEVEL_RANGE                     = new IntegerConfig("lightLevelRange", 24, 1, 64, "The block range to render the Light Level overlay in");
        public static final IntegerConfig LIGHT_LEVEL_THRESHOLD                 = new IntegerConfig("lightLevelThreshold", 8, 0, 15, "The light level threshold which is considered safe");
        public static final DoubleConfig LIGHT_LEVEL_Z_OFFSET                   = new DoubleConfig("lightLevelZOffset", 0.005, 0.0, 1.0, "The relative \"z\" offset for the light level overlay.\nMeant to help with potential z-fighting issues.\nRange: 0.0 - 1.0");
        public static final BooleanConfig MAP_PREVIEW                           = new BooleanConfig("mapPreview", false, "Enables rendering a preview of the map,\nwhen you hold shift while hovering over a map item");
        public static final IntegerConfig MAP_PREVIEW_SIZE                      = new IntegerConfig("mapPreviewSize", 160, 16, 512, "The size of the rendered map previews");
        public static final HotkeyConfig OPEN_CONFIG_GUI                        = new HotkeyConfig("openConfigGui", "H,C", "A hotkey to open the in-game Config GUI");
        public static final BooleanConfig REQUIRE_SNEAK                         = new BooleanConfig("requireSneak", false, "Require the player to be sneaking to render the info line HUD");
        public static final HotkeyConfig REQUIRED_KEY                           = new HotkeyConfig("requiredKey", "", KeyBindSettings.MODIFIER_INGAME_EMPTY, "Require holding this key to render the HUD");
        public static final HotkeyConfig SET_DISTANCE_REFERENCE_POINT           = new HotkeyConfig("setDistanceReferencePoint", "", "A hotkey to store the player's current position\nas the reference point for the distance info line type");
        public static final HotkeyConfig SHAPE_EDITOR                           = new HotkeyConfig("shapeEditor", "", "Opens the Shape Editor GUI for the selected shape");
        public static final BooleanConfig SHULKER_BOX_PREVIEW                   = new BooleanConfig("shulkerBoxPreview", false, "Enables rendering a preview of the Shulker Box contents,\nwhen you hold shift while hovering over a Shulker Box item");
        public static final BooleanConfig SHULKER_DISPLAY_BACKGROUND_COLOR      = new BooleanConfig("shulkerDisplayBgColor", true, "Enables tinting/coloring the Shulker Box display\nbackground texture with the dye color of the box");
        public static final BooleanConfig SHULKER_DISPLAY_REQUIRE_SHIFT         = new BooleanConfig("shulkerDisplayRequireShift", true, "Whether or not holding shift is required for the Shulker Box preview");
        public static final IntegerConfig SLIME_CHUNK_OVERLAY_RADIUS            = new IntegerConfig("slimeChunkOverlayRadius", -1, -1, 40, "The radius of chunks to render the slime chunk overlay in.\nValid range: -1 ... 40, where -1 = render distance");
        public static final BooleanConfig SORT_LINES_BY_LENGTH                  = new BooleanConfig("sortLinesByLength", false, "Sort the lines by their text's length");
        public static final BooleanConfig SORT_LINES_REVERSED                   = new BooleanConfig("sortLinesReversed", false, "Reverse the line sorting order");
        public static final IntegerConfig SPAWNABLE_COLUMNS_OVERLAY_RADIUS      = new IntegerConfig("spawnableColumnHeightsOverlayRadius", 40, 0, 128, "The radius (in blocks) to render the spawnable\ncolumn heights overlay in. Valid range: 0 ... 128");
        public static final IntegerConfig SPAWNABLE_SUB_CHUNK_CHECK_INTERVAL    = new IntegerConfig("spawnableSubChunkCheckInterval", 20, 1, 10000, "The interval in game ticks for the spawnable sub-chunk heightmap checks");
        public static final IntegerConfig SPAWNABLE_SUB_CHUNKS_OVERLAY_RADIUS   = new IntegerConfig("spawnableSubChunksOverlayRadius", -1, -1, 40, "The radius of chunks to render the spawnable sub-chunks\noverlay in. Valid range: -1 ... 40, where -1 = render distance");
        public static final BooleanConfig STRUCTURES_RENDER_THROUGH             = new BooleanConfig("structuresRenderThrough", false, "If enabled, then the Structure Bounding Boxes\nwill be rendered through blocks");
        public static final IntegerConfig TEXT_POS_X                            = new IntegerConfig("textPosX", 4, "Info line text X position from the screen edge (default: 4)");
        public static final IntegerConfig TEXT_POS_Y                            = new IntegerConfig("textPosY", 4, "Info line text Y position from the screen edge (default: 4)");
        public static final IntegerConfig TIME_DAY_DIVISOR                      = new IntegerConfig("timeDayDivisor", 24000, 1, Integer.MAX_VALUE, "The divisor value for the modulo of the day time,\nused for 'infoTimeDayModulo' info line");
        public static final IntegerConfig TIME_TOTAL_DIVISOR                    = new IntegerConfig("timeTotalDivisor", 24000, 1, Integer.MAX_VALUE, "The divisor value for the modulo of the total world time,\nused for 'infoTimeTotalModulo' info line");
        public static final HotkeyConfig TOGGLE_KEY                             = new HotkeyConfig("toggleKey", "H", KeyBindSettings.RELEASE_EXCLUSIVE, "The main rendering toggle key");
        public static final BooleanConfig USE_CUSTOMIZED_COORDINATES            = new BooleanConfig("useCustomizedCoordinateFormat", true, "Use the customized coordinate format string from 'coordinateFormat'");
        public static final BooleanConfig USE_FONT_SHADOW                       = new BooleanConfig("useFontShadow", false, "Use font shadow for the info line HUD text");
        public static final BooleanConfig USE_TEXT_BACKGROUND                   = new BooleanConfig("useTextBackground", true, "Use a solid background color behind the text\nfor the info line HUD text");
        public static final BooleanConfig WOOL_COUNTER_ENABLE_ALL               = new BooleanConfig("woolCounterEnableAll", true, "This overrides the 'woolCounterTypes' config,\nand enables all the colors. It's meant for quickly seeing all of them,\nwithout having to change the other config all the time.");
        public static final StringConfig WOOL_COUNTER_TYPES                     = new StringConfig("woolCounterTypes", "0-15", "This defines which wool counter colors are enabled\nfor the Carpet Wool Counters info lines.\nAccepted values are individual metadata values like 0,1,2,\nmetadata ranges like 4-6,8-11 and dye names like purple,blue,white.\nDifferent values should be separated by commas, without spaces.");

        public static final OptionListConfig<BlockGridMode> BLOCK_GRID_OVERLAY_MODE         = new OptionListConfig<>("blockGridOverlayMode", BlockGridMode.ALL, "The block grid render mode");
        public static final OptionListConfig<HudAlignment> HUD_ALIGNMENT                    = new OptionListConfig<>("hudAlignment", HudAlignment.TOP_LEFT, "The alignment of the info line HUD");
        public static final OptionListConfig<LightLevelMarkerMode> LIGHT_LEVEL_MARKER_MODE  = new OptionListConfig<>("lightLevelMarkers", LightLevelMarkerMode.SQUARE, "Which type of colored marker to use in the\nLight Level overlay, if any");
        public static final OptionListConfig<LightLevelNumberMode> LIGHT_LEVEL_NUMBER_MODE  = new OptionListConfig<>("lightLevelNumbers", LightLevelNumberMode.BLOCK, "Which light level number(s) to render in the Light Level overlay");

        public static final ImmutableList<ConfigOption<?>> OPTIONS = ImmutableList.of(
                ENABLED,
                BEACON_RANGE_AUTO_UPDATE,
                CHUNK_UNLOAD_BUCKET_HASH_SIZE,
                DEBUG_MESSAGES,
                DEBUG_RENDERER_PATH_MAX_DIST,
                DONT_RESET_SEED_ON_DIMENSION_CHANGE,
                FIX_VANILLA_DEBUG_RENDERERS,
                ITEM_NBT_ENABLED,
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
                WOOL_COUNTER_ENABLE_ALL,

                ITEM_NBT_KEY_PRETTY,
                ITEM_NBT_KEY_STRING,
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
                WOOL_COUNTER_TYPES,

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

        public static final List<HotkeyConfig> HOTKEY_LIST = ImmutableList.of(
                ITEM_NBT_KEY_PRETTY,
                ITEM_NBT_KEY_STRING,
                TOGGLE_KEY,
                REQUIRED_KEY,
                OPEN_CONFIG_GUI,
                SET_DISTANCE_REFERENCE_POINT,
                SHAPE_EDITOR
        );
    }

    public static class Colors
    {
        public static final ColorConfig BEACON_RANGE_LVL1_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl1", "0x20E060FF", "Color for the Beacon Range lvl 1 overlay");
        public static final ColorConfig BEACON_RANGE_LVL2_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl2", "0x20FFB040", "Color for the Beacon Range lvl 2 overlay");
        public static final ColorConfig BEACON_RANGE_LVL3_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl3", "0x20FFF040", "Color for the Beacon Range lvl 3 overlay");
        public static final ColorConfig BEACON_RANGE_LVL4_OVERLAY_COLOR     = new ColorConfig("beaconRangeLvl4", "0x2060FF40", "Color for the Beacon Range lvl 4 overlay");
        public static final ColorConfig BLOCK_GRID_OVERLAY_COLOR            = new ColorConfig("blockGrid", "0x80FFFFFF", "Color for the block grid overlay");
        public static final ColorConfig LIGHT_LEVEL_MARKER_DARK             = new ColorConfig("lightLevelMarkerDark", "0xFFFF4848", "The color for the spawnable spots marker");
        public static final ColorConfig LIGHT_LEVEL_MARKER_LIT              = new ColorConfig("lightLevelMarkerLit", "0xFFFFFF33", "The color for the safe (during day) spots marker");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_BLOCK_DARK       = new ColorConfig("lightLevelNumberBlockDark", "0xFFC03030", "The color for the spawnable spots number of the block light value");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_BLOCK_LIT        = new ColorConfig("lightLevelNumberBlockLit", "0xFF209040", "The color for the safe spots number of the block light value");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_SKY_DARK         = new ColorConfig("lightLevelNumberSkyDark", "0xFFFFF030", "The color for the spawnable spots number of the sky light value");
        public static final ColorConfig LIGHT_LEVEL_NUMBER_SKY_LIT          = new ColorConfig("lightLevelNumberSkyLit", "0xFF40E0FF", "The color for the safe spots number of the sky light value");
        public static final ColorConfig RANDOM_TICKS_FIXED_OVERLAY_COLOR    = new ColorConfig("randomTicksFixed", "0x40F9F225", "Color for the fixed-point random ticked chunks overlay");
        public static final ColorConfig RANDOM_TICKS_PLAYER_OVERLAY_COLOR   = new ColorConfig("randomTicksPlayer", "0x4030FE73", "Color for the player-following random ticked chunks overlay");
        public static final ColorConfig REGION_OVERLAY_COLOR                = new ColorConfig("regionFileBorders", "0x40FF8019", "Color for the region file overlay");
        public static final ColorConfig SHAPE_CAN_DESPAWN_SPHERE            = new ColorConfig("shapeCanDespawnSphere", "0x60A04050", "Default color for the \"Can Despawn Sphere\" overlay.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ColorConfig SHAPE_CAN_SPAWN_SPHERE              = new ColorConfig("shapeCanSpawnSphere", "0x60A04050", "Default color for the \"Can Spawn Sphere\" overlay.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ColorConfig SHAPE_CIRCLE                        = new ColorConfig("shapeCircle", "0x6030B0B0", "Default color for the Circle renderer.\nThe color can be changed for each shape via its configuration GUI.");
        public static final ColorConfig SHAPE_DESPAWN_SPHERE                = new ColorConfig("shapeDespawnSphere", "0x60A04050", "Default color for the \"Despawn Sphere\" overlay.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ColorConfig SHAPE_SPHERE_BLOCKY                 = new ColorConfig("shapeSphereBlocky", "0x6030B0B0", "Default color for the blocky/block-based Sphere renderer.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ColorConfig SLIME_CHUNKS_OVERLAY_COLOR          = new ColorConfig("slimeChunks", "0xB020F020", "Color for the slime chunks overlay");
        public static final ColorConfig SPAWN_PLAYER_ENTITY_OVERLAY_COLOR   = new ColorConfig("spawnPreviewAtPlayerEntity", "0x402050D0", "Color for the entity-processing spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ColorConfig SPAWN_PLAYER_LAZY_OVERLAY_COLOR     = new ColorConfig("spawnPreviewAtPlayerLazy", "0x40D030D0", "Color for the \"lazy-loaded\" spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ColorConfig SPAWN_REAL_ENTITY_OVERLAY_COLOR     = new ColorConfig("spawnChunksRealEntity", "0x4030FF20", "Color for the entity-processing real spawn chunks overlay");
        public static final ColorConfig SPAWN_REAL_LAZY_OVERLAY_COLOR       = new ColorConfig("spawnChunksRealLazy", "0x40FF3020", "Color for the \"lazy-loaded\" real spawn chunks overlay");
        public static final ColorConfig SPAWNABLE_CHUNKS_FIXED              = new ColorConfig("spawnableChunkFixed", "0x40FF2090", "Color for the location-fixed spawnable chunks overlay");
        public static final ColorConfig SPAWNABLE_CHUNKS_PLAYER             = new ColorConfig("spawnableChunksPlayer", "0x40FF3030", "Color for the player-following spawnable chunks overlay");
        public static final ColorConfig SPAWNABLE_COLUMNS_OVERLAY_COLOR     = new ColorConfig("spawnableColumnHeights", "0xA0FF00FF", "Color for the spawnable sub-chunks overlay");
        public static final ColorConfig TEXT_BACKGROUND_COLOR               = new ColorConfig("textBackgroundColor", "0xA0505050", "Info line HUD text background color");
        public static final ColorConfig TEXT_COLOR                          = new ColorConfig("textColor", "0xFFE0E0E0", "Info line text color");

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
    public Map<String, List<? extends ConfigOption<?>>> getConfigsPerCategories()
    {
        Map<String, List<? extends ConfigOption<?>>> map = new LinkedHashMap<>();

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

    @Override
    public void onPostLoad()
    {
        DataStorage.getInstance().getWoolCounters().updateEnabledCounters(Generic.WOOL_COUNTER_TYPES.getStringValue());
    }
}
