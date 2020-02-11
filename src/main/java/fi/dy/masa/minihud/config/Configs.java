package fi.dy.masa.minihud.config;

import java.io.File;
import java.util.List;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
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
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.util.BlockGridMode;
import fi.dy.masa.minihud.util.LightLevelMarkerMode;
import fi.dy.masa.minihud.util.LightLevelNumberMode;

public class Configs implements IConfigHandler
{
    private static final String CONFIG_FILE_NAME = Reference.MOD_ID + ".json";
    private static final int CONFIG_VERSION = 1;

    public static class Generic
    {
        public static final ConfigBoolean       AXOLOTL_TOOLTIPS                    = new ConfigBoolean("axolotlTooltipsTooltips", false, "Adds the Axolotl variant name to the bucket tooltip");
        public static final ConfigBoolean       BEACON_RANGE_AUTO_UPDATE            = new ConfigBoolean("beaconRangeAutoUpdate", false, "This enables the Beacon Range renderer to update automatically\nwhen block or chunk change packets are received.\nThose updates can have a performance impact if tons of blocks are changing often.\nYou can toggle off/on the renderer to update to those changes manually.");
        public static final ConfigBoolean       BEE_TOOLTIPS                        = new ConfigBoolean("beeTooltips", false, "Adds the number of contained bees to the tooltip of Bee Hive and Bee Nest items");
        public static final ConfigBoolean       HONEY_TOOLTIPS                      = new ConfigBoolean("honeyTooltips", false, "Adds the honey level to the tooltip of Bee Hive and Bee Nest items");
        public static final ConfigOptionList    BLOCK_GRID_OVERLAY_MODE             = new ConfigOptionList("blockGridOverlayMode", BlockGridMode.ALL, "The block grid render mode");
        public static final ConfigInteger       BLOCK_GRID_OVERLAY_RADIUS           = new ConfigInteger("blockGridOverlayRadius", 32, "The radius of the block grid lines to render");
        public static final ConfigString        COORDINATE_FORMAT_STRING            = new ConfigString("coordinateFormat", "x: %.1f y: %.1f z: %.1f", "The format string for the coordinate line.\nNeeds to have three %f format strings!\nDefault: x: %.1f y: %.1f z: %.1f");
        public static final ConfigString        DATE_FORMAT_REAL                    = new ConfigString("dateFormatReal", "yyyy-MM-dd HH:mm:ss", "The format string for real time, see the Java SimpleDateFormat\nclass for the format patterns, if needed.");
        public static final ConfigString        DATE_FORMAT_MINECRAFT               = new ConfigString("dateFormatMinecraft", "MC time: (day {DAY}) {HOUR}:{MIN}:xx", "The format string for the Minecraft time.\nThe supported placeholders are: {DAY_1}, {DAY}, {HOUR}, {MIN}, {SEC}.\n{DAY_1} starts the day counter from 1, {DAY} starts from 0.");
        public static final ConfigBoolean       DEBUG_MESSAGES                      = new ConfigBoolean("debugMessages", false, "Enables some debug messages in the game console");
        public static final ConfigBoolean       DEBUG_RENDERER_PATH_MAX_DIST        = new ConfigBoolean("debugRendererPathFindingEnablePointWidth", true, "If true, then the vanilla pathfinding debug renderer\nwill render the path point width boxes.");
        public static final ConfigBoolean       DONT_RESET_SEED_ON_DIMENSION_CHANGE = new ConfigBoolean("dontClearStoredSeedOnDimensionChange", true, "Don't clear the stored world seed when just changing dimensions.\nSome mods may use per-dimension seeds, so you may need to change\nthis in case the different dimensions on your server/mod pack\nhave different world seeds.");
        public static final ConfigBoolean       ENABLED                             = new ConfigBoolean("enabled", true, "The main rendering toggle for all MiniHUD rendering");
        public static final ConfigBoolean       FIX_VANILLA_DEBUG_RENDERERS         = new ConfigBoolean("enableVanillaDebugRendererFix", true, "If true, then the vanilla debug renderer OpenGL state is fixed.");
        public static final ConfigDouble        FONT_SCALE                          = new ConfigDouble("fontScale", 0.5, 0.0, 100.0, "Font scale factor for the info line HUD. Default: 0.5\n");
        public static final ConfigOptionList    HUD_ALIGNMENT                       = new ConfigOptionList("hudAlignment", HudAlignment.TOP_LEFT, "The alignment of the info line HUD");
        public static final ConfigBoolean       LIGHT_LEVEL_COLORED_NUMBERS         = new ConfigBoolean("lightLevelColoredNumbers", true, "Whether to use colored or white numbers\nfor the Light Level overlay numbers");
        public static final ConfigOptionList    LIGHT_LEVEL_MARKER_MODE             = new ConfigOptionList("lightLevelMarkers", LightLevelMarkerMode.SQUARE, "Which type of colored marker to use in the\nLight Level overlay, if any");
        public static final ConfigDouble        LIGHT_LEVEL_MARKER_SIZE             = new ConfigDouble("lightLevelMarkerSize", 0.84, 0.0, 1.0, "The size of the light level colored marker.\nRange: 0.0 - 1.0");
        public static final ConfigOptionList    LIGHT_LEVEL_NUMBER_MODE             = new ConfigOptionList("lightLevelNumbers", LightLevelNumberMode.BLOCK, "Which light level number(s) to render in the Light Level overlay");
        public static final ConfigDouble        LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_X   = new ConfigDouble("lightLevelNumberOffsetBlockX", 0.26, 0.0, 1.0, "The relative \"x\" offset for the block light level number.\nRange: 0.0 - 1.0");
        public static final ConfigDouble        LIGHT_LEVEL_NUMBER_OFFSET_BLOCK_Y   = new ConfigDouble("lightLevelNumberOffsetBlockY", 0.32, 0.0, 1.0, "The relative \"y\" offset for the block light level number.\nRange: 0.0 - 1.0");
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
        public static final ConfigInteger       SPAWNABLE_COLUMNS_OVERLAY_RADIUS    = new ConfigInteger("spawnableColumnHeightsOverlayRadius", 40, 0, 128, "The radius (in blocks) to render the spawnable\ncolumn heights overlay in. Valid range: 0 - 128");
        public static final ConfigBoolean       STRUCTURES_RENDER_THROUGH           = new ConfigBoolean("structuresRenderThrough", false, "If enabled, then the Structure Bounding Boxes\nwill be rendered through blocks");
        public static final ConfigInteger       TEXT_POS_X                          = new ConfigInteger("textPosX", 4, "Text X position from the screen edge (default: 4)");
        public static final ConfigInteger       TEXT_POS_Y                          = new ConfigInteger("textPosY", 4, "Text Y position from the screen edge (default: 4)");
        public static final ConfigInteger       TIME_DAY_DIVISOR                    = new ConfigInteger("timeDayDivisor", 24000, 1, Integer.MAX_VALUE, "The divisor value for the modulo of the day time");
        public static final ConfigInteger       TIME_TOTAL_DIVISOR                  = new ConfigInteger("timeTotalDivisor", 24000, 1, Integer.MAX_VALUE, "The divisor value for the modulo of the total world time");
        public static final ConfigHotkey        TOGGLE_KEY                          = new ConfigHotkey("toggleKey", "H", KeybindSettings.RELEASE_EXCLUSIVE, "The main toggle key");
        public static final ConfigBoolean       USE_CUSTOMIZED_COORDINATES          = new ConfigBoolean("useCustomizedCoordinateFormat", true, "Use the customized coordinate format string");
        public static final ConfigBoolean       USE_FONT_SHADOW                     = new ConfigBoolean("useFontShadow", false, "Use font shadow");
        public static final ConfigBoolean       USE_TEXT_BACKGROUND                 = new ConfigBoolean("useTextBackground", true, "Use a solid background color behind the text");

        public static final ImmutableList<IConfigBase> OPTIONS = ImmutableList.of(
                AXOLOTL_TOOLTIPS,
                BEE_TOOLTIPS,
                HONEY_TOOLTIPS,
                ENABLED,
                BEACON_RANGE_AUTO_UPDATE,
                DEBUG_MESSAGES,
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

                BLOCK_GRID_OVERLAY_RADIUS,
                COORDINATE_FORMAT_STRING,
                DATE_FORMAT_REAL,
                DATE_FORMAT_MINECRAFT,
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
        public static final ConfigColor BEACON_RANGE_LVL1_OVERLAY_COLOR     = new ConfigColor("beaconRangeLvl1", "0x20E060FF", "Color for the Beacon Range lvl 1 overlay");
        public static final ConfigColor BEACON_RANGE_LVL2_OVERLAY_COLOR     = new ConfigColor("beaconRangeLvl2", "0x20FFB040", "Color for the Beacon Range lvl 2 overlay");
        public static final ConfigColor BEACON_RANGE_LVL3_OVERLAY_COLOR     = new ConfigColor("beaconRangeLvl3", "0x20FFF040", "Color for the Beacon Range lvl 3 overlay");
        public static final ConfigColor BEACON_RANGE_LVL4_OVERLAY_COLOR     = new ConfigColor("beaconRangeLvl4", "0x2060FF40", "Color for the Beacon Range lvl 4 overlay");
        public static final ConfigColor BLOCK_GRID_OVERLAY_COLOR                = new ConfigColor("blockGridOverlayColor",              "#80FFFFFF", "Color for the block grid overlay");
        public static final ConfigColor LIGHT_LEVEL_MARKER_DARK                 = new ConfigColor("lightLevelMarkerDark",               "#FFFF4848", "The color for the spawnable spots marker");
        public static final ConfigColor LIGHT_LEVEL_MARKER_LIT                  = new ConfigColor("lightLevelMarkerLit",                "#FFFFFF33", "The color for the safe (during day) spots marker");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_BLOCK_DARK           = new ConfigColor("lightLevelNumberBlockDark",          "#FFC03030", "The color for the spawnable spots number of the block light value");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_BLOCK_LIT            = new ConfigColor("lightLevelNumberBlockLit",           "#FF209040", "The color for the safe spots number of the block light value");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_SKY_DARK             = new ConfigColor("lightLevelNumberSkyDark",            "#FFFFF030", "The color for the spawnable spots number of the sky light value");
        public static final ConfigColor LIGHT_LEVEL_NUMBER_SKY_LIT              = new ConfigColor("lightLevelNumberSkyLit",             "#FF40E0FF", "The color for the safe spots number of the sky light value");
        public static final ConfigColor RANDOM_TICKS_FIXED_OVERLAY_COLOR        = new ConfigColor("randomTicksFixedOverlayColor",       "#30F9F225", "Color for the fixed-point random ticked chunks overlay");
        public static final ConfigColor RANDOM_TICKS_PLAYER_OVERLAY_COLOR       = new ConfigColor("randomTicksPlayerOverlayColor",      "#3030FE73", "Color for the player-following random ticked chunks overlay");
        public static final ConfigColor REGION_OVERLAY_COLOR                    = new ConfigColor("regionOverlayColor",                 "#30FF8019", "Color for the region file overlay");
        public static final ConfigColor SHAPE_CAN_DESPAWN_SPHERE                = new ConfigColor("shapeCanDespawnSphere",              "#60A04050", "Default color for the \"Can Despawn Sphere\" overlay.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ConfigColor SHAPE_CAN_SPAWN_SPHERE                  = new ConfigColor("shapeCanSpawnSphere",                "#60A04050", "Default color for the \"Can Spawn Sphere\" overlay.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ConfigColor SHAPE_CIRCLE                            = new ConfigColor("shapeCircle",                        "#6030B0B0", "Default color for the Circle renderer.\nThe color can be changed for each shape via its configuration GUI.");
        public static final ConfigColor SHAPE_DESPAWN_SPHERE                    = new ConfigColor("shapeDespawnSphere",                 "#60A04050", "Default color for the \"Despawn Sphere\" overlay.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ConfigColor SHAPE_SPHERE_BLOCKY                     = new ConfigColor("shapeSphereBlocky",                  "#6030B0B0", "Default color for the blocky/block-based Sphere renderer.\nThe color can be changed for each sphere via its configuration GUI.");
        public static final ConfigColor SLIME_CHUNKS_OVERLAY_COLOR              = new ConfigColor("slimeChunksOverlayColor",            "#3020F020", "Color for the slime chunks overlay");
        public static final ConfigColor SPAWN_PLAYER_ENTITY_OVERLAY_COLOR       = new ConfigColor("spawnPlayerEntityOverlayColor",      "#302050D0", "Color for the entity-processing would-be spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_PLAYER_LAZY_OVERLAY_COLOR         = new ConfigColor("spawnPlayerLazyOverlayColor",        "#30D030D0", "Color for the \"lazy-loaded\" would-be spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_PLAYER_OUTER_OVERLAY_COLOR        = new ConfigColor("spawnPlayerOuterOverlayColor",       "#306900D2", "Color for the 1.14+ outer loaded would-be spawn chunks overlay of\nhow the spawn chunks would be if the spawn were\nto be at the player's current position");
        public static final ConfigColor SPAWN_REAL_ENTITY_OVERLAY_COLOR         = new ConfigColor("spawnRealEntityOverlayColor",        "#3030FF20", "Color for the entity-processing real spawn chunks overlay");
        public static final ConfigColor SPAWN_REAL_LAZY_OVERLAY_COLOR           = new ConfigColor("spawnRealLazyOverlayColor",          "#30FF3020", "Color for the \"lazy-loaded\" real spawn chunks overlay");
        public static final ConfigColor SPAWN_REAL_OUTER_OVERLAY_COLOR          = new ConfigColor("spawnRealOuterOverlayColor",         "#309D581A", "Color for the 1.14+ outer loaded real spawn chunks overlay");
        public static final ConfigColor SPAWNABLE_COLUMNS_OVERLAY_COLOR         = new ConfigColor("spawnableColumnHeightsOverlayColor", "#A0FF00FF", "Color for the spawnable sub-chunks overlay");
        public static final ConfigColor TEXT_BACKGROUND_COLOR                   = new ConfigColor("textBackgroundColor",                "#A0505050", "Text background color");
        public static final ConfigColor TEXT_COLOR                              = new ConfigColor("textColor",                          "#FFE0E0E0", "Info line text color");

        public static final ImmutableList<IConfigValue> OPTIONS = ImmutableList.of(
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
                SPAWN_PLAYER_OUTER_OVERLAY_COLOR,
                SPAWN_REAL_ENTITY_OVERLAY_COLOR,
                SPAWN_REAL_LAZY_OVERLAY_COLOR,
                SPAWN_REAL_OUTER_OVERLAY_COLOR,
                SPAWNABLE_COLUMNS_OVERLAY_COLOR,
                TEXT_BACKGROUND_COLOR,
                TEXT_COLOR
        );
    }

    public static void loadFromFile()
    {
        File configFile = new File(FileUtils.getConfigDirectory(), CONFIG_FILE_NAME);

        if (configFile.exists() && configFile.isFile() && configFile.canRead())
        {
            JsonElement element = JsonUtils.parseJsonFile(configFile);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();
                JsonObject objInfoLineOrders = JsonUtils.getNestedObject(root, "InfoLineOrders", false);

                ConfigUtils.readConfigBase(root, "Colors", Configs.Colors.OPTIONS);
                ConfigUtils.readConfigBase(root, "Generic", Configs.Generic.OPTIONS);
                ConfigUtils.readHotkeyToggleOptions(root, "InfoHotkeys", "InfoTypeToggles", ImmutableList.copyOf(InfoToggle.values()));
                ConfigUtils.readHotkeyToggleOptions(root, "RendererHotkeys", "RendererToggles", ImmutableList.copyOf(RendererToggle.values()));
                ConfigUtils.readConfigBase(root, "StructureColors", StructureToggle.getColorConfigs());
                ConfigUtils.readConfigBase(root, "StructureHotkeys", StructureToggle.getHotkeys());
                ConfigUtils.readConfigBase(root, "StructureToggles", StructureToggle.getToggleConfigs());

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
    }

    public static void saveToFile()
    {
        File dir = FileUtils.getConfigDirectory();

        if ((dir.exists() && dir.isDirectory()) || dir.mkdirs())
        {
            JsonObject root = new JsonObject();
            JsonObject objInfoLineOrders = JsonUtils.getNestedObject(root, "InfoLineOrders", true);

            ConfigUtils.writeConfigBase(root, "Colors", Configs.Colors.OPTIONS);
            ConfigUtils.writeConfigBase(root, "Generic", Configs.Generic.OPTIONS);
            ConfigUtils.writeHotkeyToggleOptions(root, "InfoHotkeys", "InfoTypeToggles", ImmutableList.copyOf(InfoToggle.values()));
            ConfigUtils.writeHotkeyToggleOptions(root, "RendererHotkeys", "RendererToggles", ImmutableList.copyOf(RendererToggle.values()));
            ConfigUtils.writeConfigBase(root, "StructureColors", StructureToggle.getColorConfigs());
            ConfigUtils.writeConfigBase(root, "StructureHotkeys", StructureToggle.getHotkeys());
            ConfigUtils.writeConfigBase(root, "StructureToggles", StructureToggle.getToggleConfigs());

            for (InfoToggle toggle : InfoToggle.values())
            {
                objInfoLineOrders.add(toggle.getName(), new JsonPrimitive(toggle.getIntegerValue()));
            }

            root.add("config_version", new JsonPrimitive(CONFIG_VERSION));

            JsonUtils.writeJsonToFile(root, new File(dir, CONFIG_FILE_NAME));
        }
    }

    @Override
    public void load()
    {
        loadFromFile();
    }

    @Override
    public void save()
    {
        saveToFile();
    }
}
