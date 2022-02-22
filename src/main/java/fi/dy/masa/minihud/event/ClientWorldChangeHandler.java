package fi.dy.masa.minihud.event;

import java.io.File;
import javax.annotation.Nullable;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.WorldUtils;
import fi.dy.masa.minihud.LiteModMiniHud;
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
    public void onPreClientWorldChange(@Nullable WorldClient worldBefore, @Nullable WorldClient worldAfter, Minecraft mc)
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
    public void onPostClientWorldChange(@Nullable WorldClient worldBefore, @Nullable WorldClient worldAfter, Minecraft mc)
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
        File file = getCurrentStorageFile(false);
        JsonObject root = new JsonObject();

        root.add("shapes", ShapeManager.INSTANCE.toJson());
        root.add("data_storage", DataStorage.getInstance().toJson());

        JsonUtils.writeJsonToFile(root, file);
    }

    private void readStoredDataPerDimension()
    {
        // Per-dimension file
        File file = getCurrentStorageFile(false);
        JsonElement element = JsonUtils.parseJsonFile(file);

        if (element != null)
        {
            JsonUtils.readObjectIfPresent(element, "shapes", ShapeManager.INSTANCE::fromJson);
            JsonUtils.readObjectIfPresent(element, "data_storage", DataStorage.getInstance()::fromJson);
        }
    }

    public static File getCurrentConfigDirectory()
    {
        return new File(FileUtils.getConfigDirectory(), Reference.MOD_ID);
    }

    public static File getCurrentStorageFile(boolean globalData)
    {
        File dir = getCurrentConfigDirectory();

        if (dir.exists() == false && dir.mkdirs() == false)
        {
            LiteModMiniHud.logger.warn("Failed to create the config directory '{}'", dir.getAbsolutePath());
        }

        return new File(dir, StringUtils.getStorageFileName(globalData, "", ".json", Reference.MOD_ID + "_default"));
    }
}
