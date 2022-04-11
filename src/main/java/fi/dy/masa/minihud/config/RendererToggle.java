package fi.dy.masa.minihud.config;

import java.util.Locale;
import java.util.Optional;
import java.util.stream.Collectors;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.ConfigInfo;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.malilib.input.KeyBindSettings;
import fi.dy.masa.malilib.input.callback.ToggleBooleanWithMessageKeyCallback;
import fi.dy.masa.malilib.listener.EventListener;
import fi.dy.masa.malilib.overlay.message.MessageHelpers.BooleanConfigMessageFactory;
import fi.dy.masa.malilib.util.data.ModInfo;
import fi.dy.masa.minihud.Reference;

public enum RendererToggle implements ConfigInfo
{
    DEBUG_COLLISION_BOXES               ("debugCollisionBoxEnabled"),
    DEBUG_HEIGHT_MAP                    ("debugHeightMapEnabled"),
    DEBUG_NEIGHBOR_UPDATES              ("debugNeighborsUpdateEnabled"),
    DEBUG_PATH_FINDING                  ("debugPathfindingEnabled"),
    DEBUG_SOLID_FACES                   ("debugSolidFaceEnabled"),
    DEBUG_WATER                         ("debugWaterEnabled"),

    BEACON_RANGE                        ("beaconRange"),
    BLOCK_GRID                          ("blockGrid"),
    CHUNK_UNLOAD_BUCKET                 ("chunkUnloadBucket", KeyBindSettings.INGAME_BOTH),
    LIGHT_LEVEL                         ("lightLevel"),
    RANDOM_TICKS_FIXED                  ("randomTicksFixed"),
    RANDOM_TICKS_PLAYER                 ("randomTicksPlayer"),
    REGION_FILE                         ("regionFile"),
    SHAPE_RENDERER                      ("shapeRenderer"),
    SLIME_CHUNKS                        ("slimeChunks", KeyBindSettings.INGAME_BOTH),
    SPAWNABLE_CHUNKS_FIXED              ("spawnableChunksFixed"),
    SPAWNABLE_CHUNKS_PLAYER             ("spawnableChunksPlayer"),
    SPAWNABLE_COLUMN_HEIGHTS            ("spawnableColumnHeights"),
    SPAWNER_POSITIONS                   ("spawnerPositions"),
    SPAWN_CHUNKS_REAL                   ("spawnChunkReal"),
    SPAWN_CHUNKS_PLAYER                 ("spawnChunkPlayer"),
    STRUCTURE_BOUNDING_BOXES            ("structureMainToggle"),
    WATER_FALLS                         ("waterFalls");

    public static final ImmutableList<RendererToggle> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<BooleanConfig> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(RendererToggle::getBooleanConfig).collect(Collectors.toList()));
    public static final ImmutableList<HotkeyConfig> TOGGLE_HOTKEYS = ImmutableList.copyOf(VALUES.stream().map(RendererToggle::getHotkeyConfig).collect(Collectors.toList()));

    private final BooleanConfig booleanConfig;
    private final HotkeyConfig toggleHotkey;

    RendererToggle(String name)
    {
        this(name, KeyBindSettings.INGAME_DEFAULT);
    }

    RendererToggle(String name, KeyBindSettings settings)
    {
        this.booleanConfig = new BooleanConfig(name, false);
        this.toggleHotkey = new HotkeyConfig(name, "", settings);

        String nameLower = name.toLowerCase(Locale.ROOT);
        String nameKey = "minihud.renderer_toggle.name." + nameLower;
        String commentKey = "minihud.renderer_toggle.comment." + nameLower;

        this.booleanConfig.setNameTranslationKey(nameKey);
        this.booleanConfig.setPrettyNameTranslationKey(nameKey);
        this.booleanConfig.setCommentTranslationKey(commentKey);

        this.toggleHotkey.setNameTranslationKey(nameKey);
        this.toggleHotkey.setPrettyNameTranslationKey(nameKey);
        this.toggleHotkey.setCommentTranslationKey(commentKey);

        this.toggleHotkey.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(this.booleanConfig));
    }

    public boolean isRendererEnabled()
    {
        return this.booleanConfig.getBooleanValue();
    }

    public void addValueChangeListener(EventListener listener)
    {
        this.booleanConfig.addValueChangeListener(listener);
    }

    public void addEnableListener(EventListener listener)
    {
        this.booleanConfig.addEnableListener(listener);
    }

    public void setToggleMessageFactory(@Nullable BooleanConfigMessageFactory messageFactory)
    {
        this.toggleHotkey.getKeyBind().setCallback(new ToggleBooleanWithMessageKeyCallback(this.booleanConfig, messageFactory));
    }

    public BooleanConfig getBooleanConfig()
    {
        return this.booleanConfig;
    }

    public HotkeyConfig getHotkeyConfig()
    {
        return this.toggleHotkey;
    }

    public KeyBind getKeyBind()
    {
        return this.toggleHotkey.getKeyBind();
    }

    @Override
    public ModInfo getModInfo()
    {
        return Reference.MOD_INFO;
    }

    @Override
    public String getName()
    {
        return this.booleanConfig.getName();
    }

    @Override
    public String getDisplayName()
    {
        return this.booleanConfig.getDisplayName();
    }

    @Override
    public Optional<String> getComment()
    {
        return this.booleanConfig.getComment();
    }

    @Override
    public boolean isModified()
    {
        return this.booleanConfig.isModified() || this.toggleHotkey.isModified();
    }

    @Override
    public void resetToDefault()
    {
        this.booleanConfig.resetToDefault();
        this.toggleHotkey.resetToDefault();
    }
}
