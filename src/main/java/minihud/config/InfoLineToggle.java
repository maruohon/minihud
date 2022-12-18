package minihud.config;

import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;
import com.google.common.collect.ImmutableList;

import malilib.config.option.BooleanConfig;
import malilib.config.option.ConfigInfo;
import malilib.config.option.HotkeyConfig;
import malilib.config.option.IntegerConfig;
import malilib.input.KeyBind;
import malilib.input.KeyBindSettings;
import malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import malilib.listener.EventListener;
import malilib.util.data.ModInfo;
import minihud.Reference;

public enum InfoLineToggle implements ConfigInfo
{
    BIOME                   ("infoBiomeName",               false, 19),
    BIOME_REG_NAME          ("infoBiomeRegistryName",       false, 20),
    BLOCK_BREAK_SPEED       ("infoBlockBreakSpeed",         false,  6),
    BLOCK_ENTITIES          ("infoBlockEntities",           false, 32),
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
    PLAYER_FACING           ("infoPlayerFacing",            true,   8),
    PLAYER_PITCH_ROTATION   ("infoPlayerPitchRotation",     false, 12),
    PLAYER_YAW_ROTATION     ("infoPlayerYawRotation",       false, 11),
    REGION_FILE             ("infoRegionFile",              false, 29),
    SERVER_TPS              ("infoServerTPS",               false,  9),
    SLIME_CHUNK             ("infoSlimeChunk",              false, 22),
    SPEED                   ("infoSpeed",                   false, 13),
    SPEED_AXIS              ("infoSpeedAxis",               false, 13),
    TIME_REAL               ("infoTimeIRL",                 true,   1),
    TIME_DAY_MODULO         ("infoTimeDayModulo",           false, 35),
    TIME_TOTAL_MODULO       ("infoTimeTotalModulo",         false, 34),
    TIME_WORLD              ("infoTimeWorld",               false,  2),
    TIME_WORLD_FORMATTED    ("infoWorldTimeFormatted",      false,  3);

    public static final ImmutableList<InfoLineToggle> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<BooleanConfig> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(InfoLineToggle::getBooleanConfig).collect(Collectors.toList()));
    public static final ImmutableList<HotkeyConfig> TOGGLE_HOTKEYS = ImmutableList.copyOf(VALUES.stream().map(InfoLineToggle::getHotkeyConfig).collect(Collectors.toList()));
    public static final ImmutableList<IntegerConfig> LINE_ORDER_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(InfoLineToggle::getLineOrderConfig).collect(Collectors.toList()));

    private final BooleanConfig toggleStatus;
    private final HotkeyConfig toggleHotkey;
    private final IntegerConfig lineOrder;

    InfoLineToggle(String name, boolean defaultValue, int lineOrder)
    {
        this(name, defaultValue, lineOrder, KeyBindSettings.INGAME_DEFAULT);
    }

    InfoLineToggle(String name, boolean defaultValue, int lineOrder, KeyBindSettings settings)
    {
        String nameLower = name.toLowerCase(Locale.ROOT);
        String nameKey = "minihud.info_line.name." + nameLower;
        String commentKey = "minihud.info_line.comment." + nameLower;

        this.toggleStatus = new BooleanConfig(name, defaultValue);
        this.toggleHotkey = new HotkeyConfig(name, "", settings);
        this.lineOrder = new IntegerConfig(name, lineOrder);

        this.toggleStatus.setNameTranslationKey(nameKey);
        this.toggleStatus.setPrettyNameTranslationKey(nameKey);
        this.toggleStatus.setCommentTranslationKey(commentKey);

        this.toggleHotkey.setNameTranslationKey(nameKey);
        this.toggleHotkey.setPrettyNameTranslationKey(nameKey);
        this.toggleHotkey.setCommentTranslationKey(commentKey);
        this.toggleHotkey.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(this.toggleStatus));
    }

    public void addValueChangeListener(EventListener listener)
    {
        this.toggleStatus.addValueChangeListener(listener);
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
    public ModInfo getModInfo()
    {
        return Reference.MOD_INFO;
    }

    @Override
    public String getName()
    {
        return this.toggleStatus.getName();
    }

    @Override
    public String getDisplayName()
    {
        return this.toggleStatus.getDisplayName();
    }

    @Override
    public Optional<String> getComment()
    {
        return this.toggleStatus.getComment();
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
