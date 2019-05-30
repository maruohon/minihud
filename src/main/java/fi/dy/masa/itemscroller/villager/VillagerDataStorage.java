package fi.dy.masa.itemscroller.villager;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.util.Constants;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.StringUtils;
import net.minecraft.nbt.CompressedStreamTools;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;

public class VillagerDataStorage
{
    private static final VillagerDataStorage INSTANCE = new VillagerDataStorage();

    private final Map<UUID, VillagerData> data = new HashMap<>();
    private UUID lastInteractedUUID;
    private boolean dirty;

    public static VillagerDataStorage getInstance()
    {
        return INSTANCE;
    }

    public void setLastInteractedUUID(UUID uuid)
    {
        this.lastInteractedUUID = uuid;
    }

    public boolean hasInteractionTarget()
    {
        return this.lastInteractedUUID != null;
    }

    @Nullable
    public VillagerData getDataForLastInteractionTarget()
    {
        return this.getDataFor(this.lastInteractedUUID, true);
    }

    public VillagerData getDataFor(@Nullable UUID uuid, boolean create)
    {
        VillagerData data = uuid != null ? this.data.get(uuid) : null;

        if (data == null && uuid != null && create)
        {
            this.setLastInteractedUUID(uuid);
            data = new VillagerData(uuid);
            this.data.put(uuid, data);
            this.dirty = true;
        }

        return data;
    }

    public void setTradeListPosition(int position)
    {
        VillagerData data = this.getDataFor(this.lastInteractedUUID, true);

        if (data != null)
        {
            data.setTradeListPosition(position);
            this.dirty = true;
        }
    }

    public void setLastPage(int page)
    {
        VillagerData data = this.getDataFor(this.lastInteractedUUID, true);

        if (data != null)
        {
            data.setLastPage(page);
            this.dirty = true;
        }
    }

    public void toggleFavorite(int tradeIndex)
    {
        VillagerData data = this.getDataFor(this.lastInteractedUUID, true);

        if (data != null)
        {
            data.toggleFavorite(tradeIndex);
            this.dirty = true;
        }
    }

    private void readFromNBT(NBTTagCompound nbt)
    {
        if (nbt == null || nbt.contains("VillagerData", Constants.NBT.TAG_LIST) == false)
        {
            return;
        }

        NBTTagList tagList = nbt.getList("VillagerData", Constants.NBT.TAG_COMPOUND);
        final int count = tagList.size();

        for (int i = 0; i < count; i++)
        {
            NBTTagCompound tag = tagList.getCompound(i);
            VillagerData data = VillagerData.fromNBT(tag);

            if (data != null)
            {
                this.data.put(data.getUUID(), data);
            }
        }
    }

    private NBTTagCompound writeToNBT(@Nonnull NBTTagCompound nbt)
    {
        NBTTagList tagList = new NBTTagList();

        for (VillagerData data : this.data.values())
        {
            tagList.add(data.toNBT());
        }

        nbt.put("VillagerData", tagList);

        this.dirty = false;

        return nbt;
    }

    private String getFileName()
    {
        String worldName = StringUtils.getWorldOrServerName();

        if (worldName != null)
        {
            return "villager_data_" + worldName + ".nbt";
        }

        return "villager_data.nbt";
    }

    private File getSaveDir()
    {
        return new File(FileUtils.getMinecraftDirectory(), Reference.MOD_ID);
    }

    public void readFromDisk()
    {
        this.data.clear();

        try
        {
            File saveDir = this.getSaveDir();

            if (saveDir != null)
            {
                File file = new File(saveDir, this.getFileName());

                if (file.exists() && file.isFile() && file.canRead())
                {
                    FileInputStream is = new FileInputStream(file);
                    this.readFromNBT(CompressedStreamTools.readCompressed(is));
                    is.close();
                }
            }
        }
        catch (Exception e)
        {
            ItemScroller.logger.warn("Failed to read villager data from file", e);
        }
    }

    public void writeToDisk()
    {
        if (this.dirty)
        {
            try
            {
                File saveDir = this.getSaveDir();

                if (saveDir == null)
                {
                    return;
                }

                if (saveDir.exists() == false)
                {
                    if (saveDir.mkdirs() == false)
                    {
                        ItemScroller.logger.warn("Failed to create the data storage directory '{}'", saveDir.getPath());
                        return;
                    }
                }

                File fileTmp  = new File(saveDir, this.getFileName() + ".tmp");
                File fileReal = new File(saveDir, this.getFileName());
                FileOutputStream os = new FileOutputStream(fileTmp);
                CompressedStreamTools.writeCompressed(this.writeToNBT(new NBTTagCompound()), os);
                os.close();

                if (fileReal.exists())
                {
                    fileReal.delete();
                }

                fileTmp.renameTo(fileReal);
                this.dirty = false;
            }
            catch (Exception e)
            {
                ItemScroller.logger.warn("Failed to write villager data to file!", e);
            }
        }
    }
}
