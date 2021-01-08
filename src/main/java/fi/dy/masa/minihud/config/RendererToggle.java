package fi.dy.masa.minihud.config;

import java.util.Locale;
import java.util.stream.Collectors;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.config.ConfigInfo;
import fi.dy.masa.malilib.config.ValueChangeCallback;
import fi.dy.masa.malilib.config.option.BooleanConfig;
import fi.dy.masa.malilib.config.option.HotkeyConfig;
import fi.dy.masa.malilib.input.KeyBind;
import fi.dy.masa.malilib.input.KeyBindSettings;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleDebugRenderer;
import fi.dy.masa.minihud.hotkeys.RendererToggleKeyCallback;

public enum RendererToggle implements ConfigInfo
{
    DEBUG_COLLISION_BOXES               ("debugCollisionBoxEnabled"),
    DEBUG_HEIGHT_MAP                    ("debugHeightMapEnabled"),
    DEBUG_NEIGHBOR_UPDATES              ("debugNeighborsUpdateEnabled"),
    DEBUG_PATH_FINDING                  ("debugPathfindingEnabled"),
    DEBUG_SOLID_FACES                   ("debugSolidFaceEnabled"),
    DEBUG_WATER                         ("debugWaterEnabled"),

    OVERLAY_BEACON_RANGE                ("overlayBeaconRange"),
    OVERLAY_BLOCK_GRID                  ("overlayBlockGrid"),
    OVERLAY_CHUNK_UNLOAD_BUCKET         ("overlayChunkUnloadBucket", KeyBindSettings.INGAME_BOTH),
    OVERLAY_LIGHT_LEVEL                 ("overlayLightLevel"),
    OVERLAY_RANDOM_TICKS_FIXED          ("overlayRandomTicksFixed"),
    OVERLAY_RANDOM_TICKS_PLAYER         ("overlayRandomTicksPlayer"),
    OVERLAY_REGION_FILE                 ("overlayRegionFile"),
    OVERLAY_SLIME_CHUNKS_OVERLAY        ("overlaySlimeChunks", KeyBindSettings.INGAME_BOTH),
    OVERLAY_SPAWNABLE_CHUNKS_FIXED      ("overlaySpawnableChunksFixed"),
    OVERLAY_SPAWNABLE_CHUNKS_PLAYER     ("overlaySpawnableChunksPlayer"),
    OVERLAY_SPAWNABLE_COLUMN_HEIGHTS    ("overlaySpawnableColumnHeights"),
    OVERLAY_SPAWNER_POSITIONS           ("overlaySpawnerPositions"),
    OVERLAY_SPAWN_CHUNK_OVERLAY_REAL    ("overlaySpawnChunkReal"),
    OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER  ("overlaySpawnChunkPlayer"),
    OVERLAY_STRUCTURE_MAIN_TOGGLE       ("overlayStructureMainToggle"),
    SHAPE_RENDERER                      ("shapeRenderer");

    public static final ImmutableList<RendererToggle> VALUES = ImmutableList.copyOf(values());
    public static final ImmutableList<BooleanConfig> TOGGLE_CONFIGS = ImmutableList.copyOf(VALUES.stream().map(RendererToggle::getBooleanConfig).collect(Collectors.toList()));
    public static final ImmutableList<HotkeyConfig> TOGGLE_HOTKEYS = ImmutableList.copyOf(VALUES.stream().map(RendererToggle::getHotkeyConfig).collect(Collectors.toList()));

    private final BooleanConfig toggleStatus;
    private final HotkeyConfig toggleHotkey;

    RendererToggle(String name)
    {
        this(name, KeyBindSettings.DEFAULT);
    }

    RendererToggle(String name, KeyBindSettings settings)
    {
        this.toggleStatus = new BooleanConfig(name, false);
        this.toggleHotkey = new HotkeyConfig(name, "", settings);

        if (name.startsWith("debug"))
        {
            this.toggleHotkey.getKeyBind().setCallback(new KeyCallbackToggleDebugRenderer(this));
        }
        else
        {
            this.toggleHotkey.getKeyBind().setCallback(new RendererToggleKeyCallback(this.toggleStatus));
        }

        if (name.equals("overlayStructureMainToggle"))
        {
            this.toggleStatus.setValueChangeCallback((newValue, oldValue) -> DataStorage.getInstance().getStructureStorage().requestStructureDataUpdates());
        }

        String nameLower = name.toLowerCase(Locale.ROOT);
        String nameKey = "minihud.renderer_toggle.name." + nameLower;
        this.toggleStatus.setNameTranslationKey(nameKey).setPrettyNameTranslationKey(nameKey);
        this.toggleStatus.setCommentTranslationKey("minihud.renderer_toggle.comment." + nameLower);
    }

    public boolean isRendererEnabled()
    {
        return this.toggleStatus.getBooleanValue();
    }

    public void setValueChangeCallback(ValueChangeCallback<Boolean> callback)
    {
        this.toggleStatus.setValueChangeCallback(callback);
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

    @Nullable
    @Override
    public String getComment()
    {
        return this.toggleStatus.getComment();
    }

    @Override
    public boolean isModified()
    {
        return this.toggleStatus.isModified() || this.toggleHotkey.isModified();
    }

    @Override
    public void resetToDefault()
    {
        this.toggleStatus.resetToDefault();
        this.toggleHotkey.resetToDefault();
    }
}
