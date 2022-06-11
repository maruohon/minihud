package fi.dy.masa.minihud.event;

import java.nio.file.Path;
import javax.annotation.Nullable;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.client.multiplayer.WorldClient;
import fi.dy.masa.malilib.config.util.ConfigUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.data.json.JsonUtils;
import fi.dy.masa.malilib.util.game.WorldUtils;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.data.DataStorage;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;

public class ClientWorldChangeHandler implements fi.dy.masa.malilib.event.ClientWorldChangeHandler
{
    private boolean hasCachedSeed;
    private long cachedSeed;

    @Override
    public void onPreClientWorldChange(@Nullable WorldClient worldBefore, @Nullable WorldClient worldAfter)
    {
        // Save the settings before the integrated server gets shut down
        if (worldBefore != null)
        {
            this.writeDataPerDimension();

            if (worldAfter != null)
            {
                this.hasCachedSeed = DataStorage.getInstance().hasStoredWorldSeed() && Configs.Generic.DONT_RESET_SEED_ON_DIMENSION_CHANGE.getBooleanValue();

                if (this.hasCachedSeed)
                {
                    this.cachedSeed = DataStorage.getInstance().getWorldSeed(WorldUtils.getDimensionId(worldAfter));
                }
            }
        }
        else
        {
            // Logging in to a world, load the global data
            if (worldAfter != null)
            {
                OverlayRenderer.resetRenderTimeout();
            }

            this.hasCachedSeed = false;
        }
    }

    @Override
    public void onPostClientWorldChange(@Nullable WorldClient worldBefore, @Nullable WorldClient worldAfter)
    {
        ShapeManager.INSTANCE.clear();

        // Clear the cached data
        DataStorage.getInstance().reset();

        if (worldAfter != null)
        {
            this.readStoredDataPerDimension();

            if (this.hasCachedSeed)
            {
                DataStorage.getInstance().setWorldSeed(this.cachedSeed);
                this.hasCachedSeed = false;
            }

            DataStorage.getInstance().onWorldLoad();
        }
        else
        {
            DataStorage.getInstance().onLogout();
        }

        RenderHandler.INSTANCE.setReady(worldAfter != null);
    }

    private void writeDataPerDimension()
    {
        JsonObject root = new JsonObject();

        root.add("shapes", ShapeManager.INSTANCE.toJson());
        root.add("data_storage", DataStorage.getInstance().toJson());

        JsonUtils.writeJsonToFile(root, getCurrentStorageFile(false));
    }

    private void readStoredDataPerDimension()
    {
        JsonElement element = JsonUtils.parseJsonFile(getCurrentStorageFile(false));

        if (element != null)
        {
            JsonUtils.readObjectIfExists(element, "shapes", ShapeManager.INSTANCE::fromJson);
            JsonUtils.readObjectIfExists(element, "data_storage", DataStorage.getInstance()::fromJson);
        }
    }

    public static Path getCurrentStorageFile(boolean globalData)
    {
        String fileName = StringUtils.getStorageFileName(globalData, "", ".json", Reference.MOD_ID + "_default");
        return ConfigUtils.createAndGetConfigDirectory(Reference.MOD_ID).resolve(fileName);
    }
}
