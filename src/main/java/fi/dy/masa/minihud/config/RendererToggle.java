package fi.dy.masa.minihud.config;

import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;
import fi.dy.masa.malilib.config.ConfigType;
import fi.dy.masa.malilib.config.IConfigBoolean;
import fi.dy.masa.malilib.hotkeys.IHotkey;
import fi.dy.masa.malilib.hotkeys.IKeybind;
import fi.dy.masa.malilib.hotkeys.KeybindMulti;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleDebugRenderer;
import fi.dy.masa.minihud.hotkeys.KeyCallbackToggleRenderer;

public enum RendererToggle implements IConfigBoolean, IHotkey
{
    DEBUG_COLLISION_BOXES               ("debugCollisionBoxEnabled",    "H,1", "Toggles the vanilla Block Collision Boxes debug renderer", "Block Collision Boxes"),
    DEBUG_HEIGHT_MAP                    ("debugHeightMapEnabled",       "H,2", "Toggles the vanilla Height Map debug renderer", "Height Map"),
    DEBUG_NEIGHBOR_UPDATES              ("debugNeighborsUpdateEnabled", "H,3", "Toggles the vanilla Block Neighbor Updates debug renderer", "Block Neighbor Updates"),
    DEBUG_PATH_FINDING                  ("debugPathfindingEnabled",     "H,4", "Toggles the vanilla Pathfinding debug renderer", "Pathfinding"),
    DEBUG_SOLID_FACES                   ("debugSolidFaceEnabled",       "H,5", "Toggles the vanilla Block Solid Faces debug renderer", "Block Solid Faces"),
    DEBUG_WATER                         ("debugWaterEnabled",           "H,6", "Toggles the vanilla Water debug renderer", "Water"),

    OVERLAY_REGION_FILE                 ("overlayRegionFile",           "H,J", "Toggle the region file border overlay renderer", "Region file border overlay"),
    OVERLAY_CHUNK_UNLOAD_BUCKET         ("overlayChunkUnloadBucket",    "H,X", "Toggle the Chunk unload bucket/priority renderer", "Chunk unload priority overlay"),
    OVERLAY_SLIME_CHUNKS_OVERLAY        ("overlaySlimeChunks",          "H,M", "Toggle the Slime Chunk overlay renderer", "Slime chunk overlay"),
    OVERLAY_SPAWN_CHUNK_OVERLAY_REAL    ("overlaySpawnChunkReal",       "H,V", "Toggle the spawn chunks overlay renderer", "Spawn chunks overlay"),
    OVERLAY_SPAWN_CHUNK_OVERLAY_PLAYER  ("overlaySpawnChunkPlayer",     "H,Q", "Toggle the pseudo (player-following) spawn chunks overlay renderer", "Would-be (player-following) spawn chunks overlay");

    private final String name;
    private final String prettyName;
    private final String comment;
    private final IKeybind keybind;
    private final boolean defaultValueBoolean;
    private boolean valueBoolean;

    private RendererToggle(String name, String defaultHotkey, String comment, String prettyName)
    {
        this.name = name;
        this.prettyName = prettyName;
        this.comment = comment;
        this.defaultValueBoolean = false;
        this.keybind = KeybindMulti.fromStorageString(defaultHotkey);

        if (name.startsWith("debug"))
        {
            this.keybind.setCallback(new KeyCallbackToggleDebugRenderer(this));
        }
        else
        {
            this.keybind.setCallback(new KeyCallbackToggleRenderer(this));
        }
    }

    @Override
    public ConfigType getType()
    {
        return ConfigType.BOOLEAN;
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
        return comment != null ? this.comment : "";
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
        this.valueBoolean = value;
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
            LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
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
                LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName());
            }
        }
        catch (Exception e)
        {
            LiteModMiniHud.logger.warn("Failed to read config value for {} from the JSON config", this.getName(), e);
        }
    }

    @Override
    public JsonElement getAsJsonElement()
    {
        return new JsonPrimitive(this.valueBoolean);
    }
}
