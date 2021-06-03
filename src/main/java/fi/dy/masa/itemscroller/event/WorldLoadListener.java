package fi.dy.masa.itemscroller.event;

import javax.annotation.Nullable;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.world.ClientWorld;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.recipes.RecipeStorage;
import fi.dy.masa.itemscroller.util.ClickPacketBuffer;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.malilib.interfaces.IWorldLoadListener;

public class WorldLoadListener implements IWorldLoadListener
{
    @Override
    public void onWorldLoadPre(@Nullable ClientWorld worldBefore, @Nullable ClientWorld worldAfter, MinecraftClient mc)
    {
        // Quitting to main menu, save the settings before the integrated server gets shut down
        if (worldBefore != null && worldAfter == null)
        {
            this.writeData();
            VillagerDataStorage.getInstance().writeToDisk();
        }
    }

    @Override
    public void onWorldLoadPost(@Nullable ClientWorld worldBefore, @Nullable ClientWorld worldAfter, MinecraftClient mc)
    {
        // Logging in to a world, load the data
        if (worldBefore == null && worldAfter != null)
        {
            this.readStoredData();
            VillagerDataStorage.getInstance().readFromDisk();
        }

        // Logging out
        if (worldAfter == null)
        {
            ClickPacketBuffer.reset();
        }
    }

    private void writeData()
    {
        if (Configs.Generic.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getBooleanValue())
        {
            RecipeStorage.getInstance().writeToDisk();
        }
    }

    private void readStoredData()
    {
        if (Configs.Generic.SCROLL_CRAFT_STORE_RECIPES_TO_FILE.getBooleanValue())
        {
            RecipeStorage.getInstance().readFromDisk();
        }
   }
}
