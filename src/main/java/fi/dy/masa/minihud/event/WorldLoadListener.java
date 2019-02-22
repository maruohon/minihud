package fi.dy.masa.minihud.event;

import java.io.File;
import javax.annotation.Nullable;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import fi.dy.masa.malilib.interfaces.IWorldLoadListener;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.JsonUtils;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.WorldUtils;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.renderer.shapes.ShapeManager;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;

public class WorldLoadListener implements IWorldLoadListener
{
    @Override
    public void onWorldLoadPre(@Nullable WorldClient world, Minecraft mc)
    {
        // Save the settings before the integrated server gets shut down
        if (Minecraft.getMinecraft().world != null)
        {
            File file = getCurrentStorageFile(false);
            JsonObject root = new JsonObject();
            root.add("shapes", ShapeManager.INSTANCE.toJson());
            JsonUtils.writeJsonToFile(root, file);
        }
    }

    @Override
    public void onWorldLoadPost(@Nullable WorldClient world, Minecraft mc)
    {
        ShapeManager.INSTANCE.clear();

        if (world != null)
        {
            File file = getCurrentStorageFile(false);
            JsonElement element = JsonUtils.parseJsonFile(file);

            if (element != null && element.isJsonObject())
            {
                JsonObject root = element.getAsJsonObject();

                if (JsonUtils.hasObject(root, "shapes"))
                {
                    ShapeManager.INSTANCE.fromJson(JsonUtils.getNestedObject(root, "shapes", false));
                }
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
            LiteModMiniHud.logger.warn("Failed to create the config directory '{}'", dir.getAbsolutePath());
        }

        return new File(dir, getStorageFileName(globalData));
    }

    private static String getStorageFileName(boolean globalData)
    {
        Minecraft mc = Minecraft.getMinecraft();
        String name = StringUtils.getWorldOrServerName();

        if (name != null)
        {
            if (globalData)
            {
                return name + ".json";
            }
            else
            {
                return name + "_dim" + WorldUtils.getDimensionId(mc.world) + ".json";
            }
        }

        return Reference.MOD_ID + "_default.json";
    }
}
