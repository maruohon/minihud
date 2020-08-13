package fi.dy.masa.minihud.config;

import java.util.Locale;
import java.util.stream.Collectors;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ValueChangeCallback;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ConfigInfo;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.config.option.IntegerConfig;
import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.malilib.input.KeyBindSettings;
import fi.dy.masa.malilib.input.callback.ToggleBooleanKeyCallback;

public enum InfoLine implements ConfigInfo
{
    BIOME                   ("infoBiome",                   false, 19),
    BIOME_REG_NAME          ("infoBiomeRegistryName",       false, 20),
    BLOCK_BREAK_SPEED       ("infoBlockBreakSpeed",         false,  6),
    BLOCK_IN_CHUNK          ("infoBlockInChunk",            false, 28),
    BLOCK_POS               ("infoBlockPosition",           false,  6),
    BLOCK_PROPS             ("infoBlockProperties",         false, 27),
    CARPET_WOOL_COUNTERS    ("infoCarpetWoolCounters",      false, 50),
    CHUNK_POS               ("infoChunkPosition",           false,  7),
    CHUNK_SECTIONS          ("infoChunkSections",           false, 14),
    CHUNK_SECTIONS_FULL     ("infoChunkSectionsLine",       false, 15),
    CHUNK_UNLOAD_ORDER      ("infoChunkUnloadOrder",        false, 30),
    CHUNK_UPDATES           ("infoChunkUpdates",            false, 16),
    COORDINATES             ("infoCoordinates",             true,   4),
    DIFFICULTY              ("infoDifficulty",              false, 18),
    DIMENSION               ("infoDimensionId",             false,  5),
    DISTANCE                ("infoDistance",                false, 33),
    ENTITIES                ("infoEntities",                false, 21),
    ENTITIES_CLIENT_WORLD   ("infoEntitiesClientWorld",     false, 22),
    ENTITY_REG_NAME         ("infoEntityRegistryName",      false, 24),
    FACING                  ("infoFacing",                  true,   8),
    FPS                     ("infoFPS",                     false,  0),
    LIGHT_LEVEL             ("infoLightLevel",              false, 10),
    LOADED_CHUNKS_COUNT     ("infoLoadedChunksCount",       false, 31),
    LOOKING_AT_BLOCK        ("infoLookingAtBlock",          false, 25),
    LOOKING_AT_BLOCK_CHUNK  ("infoLookingAtBlockInChunk",   false, 26),
    LOOKING_AT_ENTITY       ("infoLookingAtEntity",         false, 23),
    MEMORY_USAGE            ("infoMemoryUsage",             false,  0),
    MOB_CAPS                ("infoMobCaps",                 false, 40),
    PARTICLE_COUNT          ("infoParticleCount",           false, 17),
    PING                    ("infoPing",                    false, 36),
    REGION_FILE             ("infoRegionFile",              false, 29),
    ROTATION_PITCH          ("infoRotationPitch",           false, 12),
    ROTATION_YAW            ("infoRotationYaw",             false, 11),
    SERVER_TPS              ("infoServerTPS",               false,  9),
    SLIME_CHUNK             ("infoSlimeChunk",              false, 22),
    SPAWNABLE_SUB_CHUNKS    ("infoSpawnableSubChunks",      false, 32),
    SPEED                   ("infoSpeed",                   false, 13),
    SPEED_AXIS              ("infoSpeedAxis",               false, 13),
    TILE_ENTITIES           ("infoTileEntities",            false, 32),
    TIME_DAY_MODULO         ("infoTimeDayModulo",           false, 35),
    TIME_REAL               ("infoTimeIRL",                 true,   1),
    TIME_TOTAL_MODULO       ("infoTimeTotalModulo",         false, 34),
    TIME_WORLD              ("infoTimeWorld",               false,  2),
    TIME_WORLD_FORMATTED    ("infoWorldTimeFormatted",      false,  3);

    public static final ImmutableList<InfoLine> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<BooleanConfig> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(InfoLine::getBooleanConfig).collect(Collectors.toList()));
    public static final ImmutableList<HotkeyConfig> TOGGLE_HOTKEYS = ImmutableList.copyOf(VALUES.stream().map(InfoLine::getHotkeyConfig).collect(Collectors.toList()));
    public static final ImmutableList<IntegerConfig> LINE_ORDER_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(InfoLine::getLineOrderConfig).collect(Collectors.toList()));

    private final BooleanConfig toggleStatus;
    private final HotkeyConfig toggleHotkey;
    private final IntegerConfig lineOrder;

    InfoLine(String name, boolean defaultValue, int lineOrder)
    {
        this(name, defaultValue, lineOrder, KeyBindSettings.DEFAULT);
    }

    InfoLine(String name, boolean defaultValue, int lineOrder, KeyBindSettings settings)
    {
        this.toggleStatus = new BooleanConfig(name, defaultValue);
        this.toggleHotkey = new HotkeyConfig(name, "", settings);
        this.lineOrder = new IntegerConfig(name, lineOrder);
        this.toggleHotkey.getKeyBind().setCallback(new ToggleBooleanKeyCallback(this.toggleStatus));

        String nameLower = name.toLowerCase(Locale.ROOT);
        String nameKey = "minihud.info_line.name." + nameLower;
        this.toggleStatus.setNameTranslationKey(nameKey).setPrettyNameTranslationKey(nameKey);
        this.toggleStatus.setCommentTranslationKey("minihud.info_line.comment." + nameLower);
    }

    public void setValueChangeCallback(ValueChangeCallback<Boolean> callback)
    {
        this.toggleStatus.setValueChangeCallback(callback);
    }

    public boolean getBooleanValue()
    {
        return this.toggleStatus.getBooleanValue();
    }

    public int getLineOrder()
    {
        return this.lineOrder.getIntegerValue();
    }

    public BooleanConfig getBooleanConfig()
    {
        return this.toggleStatus;
    }

    public HotkeyConfig getHotkeyConfig()
    {
        return this.toggleHotkey;
    }

    public KeyBind getKeyBind()
    {
        return this.toggleHotkey.getKeyBind();
    }

    public IntegerConfig getLineOrderConfig()
    {
        return this.lineOrder;
    }

    @Override
    public String getName()
    {
        return this.toggleStatus.getName();
    }

    @Override
    public String getConfigNameTranslationKey()
    {
        return this.toggleStatus.getConfigNameTranslationKey();
    }

    @Nullable
    @Override
    public String getCommentTranslationKey()
    {
        return this.toggleStatus.getCommentTranslationKey();
    }

    @Override
    public boolean isModified()
    {
        return this.toggleStatus.isModified() ||
               this.toggleHotkey.isModified() ||
               this.lineOrder.isModified();
    }

    @Override
    public void resetToDefault()
    {
        this.toggleStatus.resetToDefault();
        this.toggleHotkey.resetToDefault();
        this.lineOrder.resetToDefault();
    }
}
