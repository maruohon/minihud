package minihud.event;

import java.nio.file.Path;
import javax.annotation.Nullable;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;

import net.minecraft.client.multiplayer.WorldClient;

import malilib.config.util.ConfigUtils;
import malilib.util.StringUtils;
import malilib.util.data.json.JsonUtils;
import minihud.Reference;
import minihud.data.DataStorage;
import minihud.renderer.OverlayRenderer;
import minihud.renderer.shapes.ShapeManager;

public class ClientWorldChangeHandler implements malilib.event.ClientWorldChangeHandler
{
    @Override
    public void onPreClientWorldChange(@Nullable WorldClient worldBefore, @Nullable WorldClient worldAfter)
    {
        // Save the settings before the world reference changes or the integrated server gets shut down
        if (worldBefore != null)
        {
            this.writeDataPerDimension();
        }
        else if (worldAfter != null)
        {
            OverlayRenderer.resetRenderTimeout();
        }
    }

    @Override
    public void onPostClientWorldChange(@Nullable WorldClient worldBefore, @Nullable WorldClient worldAfter)
    {
        // Clear the cached data
        DataStorage.INSTANCE.clear(worldAfter == null);

        if (worldAfter != null)
        {
            this.readStoredDataPerDimension();
            DataStorage.INSTANCE.afterWorldLoad();
        }
    }

    private void writeDataPerDimension()
    {
        JsonObject root = new JsonObject();

        root.add("shapes", ShapeManager.INSTANCE.toJson());
        root.add("data_storage", DataStorage.INSTANCE.toJson());

        JsonUtils.writeJsonToFile(root, getCurrentStorageFile(false));
    }

    private void readStoredDataPerDimension()
    {
        JsonElement element = JsonUtils.parseJsonFile(getCurrentStorageFile(false));

        if (element != null)
        {
            JsonUtils.readObjectIfExists(element, "shapes", ShapeManager.INSTANCE::fromJson);
            JsonUtils.readObjectIfExists(element, "data_storage", DataStorage.INSTANCE::fromJson);
        }
    }

    public static Path getCurrentStorageFile(boolean globalData)
    {
        String fileName = StringUtils.getStorageFileName(globalData, "", ".json", Reference.MOD_ID + "_default");
        return ConfigUtils.createAndGetConfigDirectory(Reference.MOD_ID).resolve(fileName);
    }
}
