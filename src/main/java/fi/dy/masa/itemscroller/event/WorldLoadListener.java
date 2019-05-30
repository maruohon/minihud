package fi.dy.masa.itemscroller.event;

import javax.annotation.Nullable;
import fi.dy.masa.itemscroller.villager.VillagerDataStorage;
import fi.dy.masa.malilib.interfaces.IWorldLoadListener;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;

public class WorldLoadListener implements IWorldLoadListener
{
    @Override
    public void onWorldLoadPre(@Nullable WorldClient world, Minecraft mc)
    {
        WorldClient worldOld = Minecraft.getInstance().world;

        // Save the settings before the integrated server gets shut down
        if (worldOld != null)
        {
            // Quitting to main menu
            if (world == null)
            {
                this.writeDataGlobal();
            }
        }
        else
        {
            // Logging in to a world, load the global data
            if (world != null)
            {
                this.readStoredDataGlobal();
            }
        }
    }

    @Override
    public void onWorldLoadPost(@Nullable WorldClient world, Minecraft mc)
    {
    }

    private void writeDataGlobal()
    {
        VillagerDataStorage.getInstance().writeToDisk();
    }

    private void readStoredDataGlobal()
    {
        VillagerDataStorage.getInstance().readFromDisk();
    }
}
