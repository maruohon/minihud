package fi.dy.masa.minihud.event;

import java.io.File;
import javax.annotation.Nullable;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.world.ClientWorld;
import fi.dy.masa.malilib.interfaces.IWorldLoadListener;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.renderer.OverlayRenderer;
import fi.dy.masa.minihud.renderer.RenderContainer;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import fi.dy.masa.minihud.util.DataStorage;

public class WorldLoadListener implements IWorldLoadListener
{
    private boolean hasCachedSeed;
    private long cachedSeed;
    private boolean renderersRead;

    @Override
    public void onWorldLoadPre(@Nullable ClientWorld worldBefore, @Nullable ClientWorld worldAfter, MinecraftClient mc)
    {
        // Save the settings before the integrated server gets shut down
        if (worldBefore != null)
        {
            this.writeDataPerDimension();

            // Quitting to main menu
            if (worldAfter == null)
            {
                this.writeDataGlobal();
            }

            this.hasCachedSeed = worldAfter != null && Configs.Generic.DONT_RESET_SEED_ON_DIMENSION_CHANGE.getBooleanValue();

            if (this.hasCachedSeed)
            {
                this.cachedSeed = DataStorage.getInstance().getWorldSeed(worldAfter.method_27983());
            }
        }
        else
        {
            this.hasCachedSeed = false;
        }
    }

    @Override
    public void onWorldLoadPost(@Nullable ClientWorld worldBefore, @Nullable ClientWorld worldAfter, MinecraftClient mc)
    {
        // Clear the cached data
        DataStorage.getInstance().reset();

        // Logging in to a world or changing dimensions or respawning
        if (worldAfter != null)
        {
            // Logging in to a world, load the stored data
            if (worldBefore == null)
            {
                this.readStoredDataGlobal();
            }

            this.readStoredDataPerDimension();
            OverlayRenderer.resetRenderTimeout();

            if (this.hasCachedSeed)
            {
                DataStorage.getInstance().setWorldSeed(this.cachedSeed);
                this.hasCachedSeed = false;
            }

            DataStorage.getInstance().onWorldJoin();
        }
    }

    private void writeDataPerDimension()
    {
        File file = getCurrentStorageFile(false);
        JsonObject root = new JsonObject();

        root.add("shapes", ShapeManager.INSTANCE.toJson());
        root.add("data_storage", DataStorage.getInstance().toJson());

        JsonUtils.writeJsonToFile(root, file);
    }

    private void writeDataGlobal()
    {
        File file = getCurrentStorageFile(true);
        JsonObject root = new JsonObject();

        root.add("renderers", RenderContainer.INSTANCE.toJson());

        JsonUtils.writeJsonToFile(root, file);
    }

    private void readStoredDataPerDimension()
    {
        // Per-dimension file
        File file = getCurrentStorageFile(false);
        JsonElement element = JsonUtils.parseJsonFile(file);

        if (element != null && element.isJsonObject())
        {
            JsonObject root = element.getAsJsonObject();

            if (JsonUtils.hasObject(root, "shapes"))
            {
                ShapeManager.INSTANCE.fromJson(JsonUtils.getNestedObject(root, "shapes", false));
            }

            if (JsonUtils.hasObject(root, "data_storage"))
            {
                DataStorage.getInstance().fromJson(JsonUtils.getNestedObject(root, "data_storage", false));
            }
        }
    }

    private void readStoredDataGlobal()
    {
        // Global file
        File file = getCurrentStorageFile(true);
        JsonElement element = JsonUtils.parseJsonFile(file);

        if (element != null && element.isJsonObject())
        {
            JsonObject root = element.getAsJsonObject();

            if (this.renderersRead == false && JsonUtils.hasObject(root, "renderers"))
            {
                RenderContainer.INSTANCE.fromJson(JsonUtils.getNestedObject(root, "renderers", false));
            }
        }
    }

    public static File getCurrentConfigDirectory()
    {
        return new File(FileUtils.getConfigDirectory(), Reference.MOD_ID);
    }

    private static File getCurrentStorageFile(boolean globalData)
    {
        File dir = getCurrentConfigDirectory();

        if (dir.exists() == false && dir.mkdirs() == false)
        {
            MiniHUD.logger.warn("Failed to create the config directory '{}'", dir.getAbsolutePath());
        }

        return new File(dir, StringUtils.getStorageFileName(globalData, "", ".json", Reference.MOD_ID + "_default"));
    }
}
