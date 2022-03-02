package fi.dy.masa.minihud.config;

import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.MinecraftClient;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.config.IConfigNotifiable;
import fi.dy.masa.malilib.config.IHotkeyTogglable;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeyCallbackToggleBooleanConfigWithMessage;
import fi.dy.masa.malilib.hotkeys.KeybindMulti;
import fi.dy.masa.malilib.hotkeys.KeybindSettings;
import fi.dy.masa.malilib.interfaces.IValueChangeCallback;
import fi.dy.masa.malilib.network.ClientPacketChannelHandler;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleRenderer;
import fi.dy.masa.minihud.network.StructurePacketHandlerCarpet;
import fi.dy.masa.minihud.network.StructurePacketHandlerServux;
import fi.dy.masa.minihud.util.DataStorage;

public enum RendererToggle implements IHotkeyTogglable, IConfigNotifiable<IConfigBoolean>
{
    OVERLAY_BEACON_RANGE                ("overlayBeaconRange",          "", "Toggle the Beacon Range overlay renderer", "Beacon Range overlay"),
    OVERLAY_BIOME_BORDER                ("overlayBiomeBorder",          "", "Toggle the Biome Border overlay renderer", "Biome Border overlay"),
    OVERLAY_BLOCK_GRID                  ("overlayBlockGrid",            "", "Toggle the Block Grid overlay renderer", "Block Grid overlay"),
    OVERLAY_CONDUIT_RANGE               ("overlayConduitRange",         "", "Toggle the Conduit Range overlay renderer", "Conduit Range overlay"),
    OVERLAY_LIGHT_LEVEL                 ("overlayLightLevel",           "", "Toggle the Light Level overlay renderer", "Light Level overlay"),
    OVERLAY_RANDOM_TICKS_FIXED          ("overlayRandomTicksFixed",     "", "Toggle the fixed-point random ticked chunks overlay renderer", "Random Ticked Chunks (fixed) overlay"),
    OVERLAY_RANDOM_TICKS_PLAYER         ("overlayRandomTicksPlayer",    "", "Toggle the player-following random ticked chunks overlay renderer", "Random Ticked Chunks (player-following) overlay"),
    OVERLAY_REGION_FILE                 ("overlayRegionFile",           "", "Toggle the region file border overlay renderer", "Region File Border overlay"),
    OVERLAY_SLIME_CHUNKS_OVERLAY        ("overlaySlimeChunks",          "", KeybindSettings.INGAME_BOTH, "Toggle the Slime Chunk overlay renderer", "Slime Chunks overlay"),
    OVERLAY_SPAWNABLE_COLUMN_HEIGHTS    ("overlaySpawnableColumnHeights","", "Toggle the spawnable column heights overlay renderer", "Spawnable column heights overlay"),
    OVERLAY_SPAWN_CHUNK_OVERLAY_REAL    ("overlaySpawnChunkReal",       "", "Toggle the spawn chunks overlay renderer", "Spawn Chunks (real) overlay"),
    OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER  ("overlaySpawnChunkPlayer",     "", "Toggle the pseudo (player-following) spawn chunks overlay renderer", "Spawn Chunks (player-following, would-be) overlay"),
    OVERLAY_STRUCTURE_MAIN_TOGGLE       ("overlayStructureMainToggle",  "", "Main toggle for all structure bounding boxes", "Structure Bounding Boxes main"),
    SHAPE_RENDERER                      ("shapeRenderer",               "", "The main toggle for the shape renderer", "Shape Renderer"),

    DEBUG_CHUNK_BORDER                  ("debugChunkBorder",            "", "Toggles the vanilla Chunk Border debug renderer", "Chunk Border"),
    DEBUG_CHUNK_INFO                    ("debugChunkInfo",              "", "Toggles the vanilla Chunk Info debug renderer", "Chunk Info"),
    DEBUG_CHUNK_OCCLUSION               ("debugChunkOcclusion",         "", "Toggles the vanilla Chunk Occlusion debug renderer", "Chunk Occlusion"),
    DEBUG_COLLISION_BOXES               ("debugCollisionBoxEnabled",    "", "Toggles the vanilla Block Collision Boxes debug renderer", "Block Collision Boxes"),
    DEBUG_NEIGHBOR_UPDATES              ("debugNeighborsUpdateEnabled", "", "Toggles the vanilla Block Neighbor Updates debug renderer", "Block Neighbor Updates"),
    DEBUG_PATH_FINDING                  ("debugPathfindingEnabled",     "", "Toggles the vanilla Pathfinding debug renderer", "Pathfinding"),
    DEBUG_SOLID_FACES                   ("debugSolidFaceEnabled",       "", "Toggles the vanilla Block Solid Faces debug renderer", "Block Solid Faces"),
    DEBUG_WATER                         ("debugWaterEnabled",           "", "Toggles the vanilla Water debug renderer", "Water");

    public static final ImmutableList<RendererToggle> VALUES = ImmutableList.copyOf(values());

    private final String name;
    private final String prettyName;
    private final String comment;
    private final IKeybind keybind;
    private final boolean defaultValueBoolean;
    private boolean valueBoolean;
    @Nullable private IValueChangeCallback<IConfigBoolean> callback;

    RendererToggle(String name, String defaultHotkey, String comment, String prettyName)
    {
        this(name, defaultHotkey, KeybindSettings.DEFAULT, comment, prettyName);
    }

    RendererToggle(String name, String defaultHotkey, KeybindSettings settings, String comment, String prettyName)
    {
        this.name = name;
        this.prettyName = prettyName;
        this.comment = comment;
        this.defaultValueBoolean = false;
        this.keybind = KeybindMulti.fromStorageString(defaultHotkey, settings);

        if (name.startsWith("debug"))
        {
            this.keybind.setCallback(new KeyCallbackToggleBooleanConfigWithMessage(this));
        }
        else
        {
            this.keybind.setCallback(new KeyCallbackToggleRenderer(this));
        }

        if (name.equals("overlayStructureMainToggle"))
        {
            this.setValueChangeCallback((config) ->
            {
                MinecraftClient mc = MinecraftClient.getInstance();

                if (mc != null && mc.player != null)
                {
                    if (mc.isIntegratedServerRunning() == false)
                    {
                        if (this.getBooleanValue())
                        {
                            DataStorage.getInstance().registerStructureChannel();
                        }
                        else
                        {
                            ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerCarpet.INSTANCE);
                            ClientPacketChannelHandler.getInstance().unregisterClientChannelHandler(StructurePacketHandlerServux.INSTANCE);
                        }
                    }
                    else
                    {
                        DataStorage.getInstance().setStructuresNeedUpdating();
                    }
                }
            });
        }
    }

    @Override
    public ConfigType getType()
    {
        return ConfigType.HOTKEY;
    }

    @Override
    public String getName()
    {
        return this.name;
    }

    @Override
    public String getPrettyName()
    {
        return this.prettyName;
    }

    @Override
    public String getStringValue()
    {
        return String.valueOf(this.valueBoolean);
    }

    @Override
    public String getDefaultStringValue()
    {
        return String.valueOf(this.defaultValueBoolean);
    }

    @Override
    public String getComment()
    {
        return StringUtils.getTranslatedOrFallback("config.comment." + this.getName().toLowerCase(), this.comment);
    }

    @Override
    public boolean getBooleanValue()
    {
        return this.valueBoolean;
    }

    @Override
    public boolean getDefaultBooleanValue()
    {
        return this.defaultValueBoolean;
    }

    @Override
    public void setBooleanValue(boolean value)
    {
        boolean oldValue = this.valueBoolean;
        this.valueBoolean = value;

        if (oldValue != this.valueBoolean)
        {
            this.onValueChanged();
        }
    }

    @Override
    public void setValueChangeCallback(IValueChangeCallback<IConfigBoolean> callback)
    {
        this.callback = callback;
    }

    @Override
    public void onValueChanged()
    {
        if (this.callback != null)
        {
            this.callback.onValueChanged(this);
        }
    }

    @Override
    public IKeybind getKeybind()
    {
        return this.keybind;
    }

    @Override
    public boolean isModified()
    {
        return this.valueBoolean != this.defaultValueBoolean;
    }

    @Override
    public boolean isModified(String newValue)
    {
        return String.valueOf(this.defaultValueBoolean).equals(newValue) == false;
    }

    @Override
    public void resetToDefault()
    {
        this.valueBoolean = this.defaultValueBoolean;
    }

    @Override
    public void setValueFromString(String value)
    {
        try
        {
            this.valueBoolean = Boolean.parseBoolean(value);
        }
        catch (Exception e)
        {
            MiniHUD.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
        }
    }

    @Override
    public void setValueFromJsonElement(JsonElement element)
    {
        try
        {
            if (element.isJsonPrimitive())
            {
                this.valueBoolean = element.getAsBoolean();
            }
            else
            {
                MiniHUD.logger.warn("Failed to read config value for {} from the JSON config", this.getName());
            }
        }
        catch (Exception e)
        {
            MiniHUD.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
        }
    }

    @Override
    public JsonElement getAsJsonElement()
    {
        return new JsonPrimitive(this.valueBoolean);
    }
}
