package fi.dy.masa.itemscroller.villager;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtIo;
import fi.dy.masa.itemscroller.ItemScroller;
import fi.dy.masa.itemscroller.Reference;
import fi.dy.masa.itemscroller.util.Constants;
import fi.dy.masa.malilib.util.FileUtils;
import fi.dy.masa.malilib.util.StringUtils;

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

    public void toggleFavorite(int tradeIndex)
    {
        VillagerData data = this.getDataFor(this.lastInteractedUUID, true);

        if (data != null)
        {
            data.toggleFavorite(tradeIndex);
            this.dirty = true;
        }
    }

    private void readFromNBT(CompoundTag nbt)
    {
        if (nbt == null || nbt.contains("VillagerData", Constants.NBT.TAG_LIST) == false)
        {
            return;
        }

        ListTag tagList = nbt.getList("VillagerData", Constants.NBT.TAG_COMPOUND);
        final int count = tagList.size();

        for (int i = 0; i < count; i++)
        {
            CompoundTag tag = tagList.getCompound(i);
            VillagerData data = VillagerData.fromNBT(tag);

            if (data != null)
            {
                this.data.put(data.getUUID(), data);
            }
        }
    }

    private CompoundTag writeToNBT(@Nonnull CompoundTag nbt)
    {
        ListTag tagList = new ListTag();

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
            File file = new File(saveDir, this.getFileName());

            if (file.exists() && file.isFile() && file.canRead())
            {
                FileInputStream is = new FileInputStream(file);
                this.readFromNBT(NbtIo.readCompressed(is));
                is.close();
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

                if (saveDir.exists() == false && saveDir.mkdirs() == false)
                {
                    ItemScroller.logger.warn("Failed to create the data storage directory '{}'", saveDir.getPath());
                    return;
                }

                File fileTmp  = new File(saveDir, this.getFileName() + ".tmp");
                File fileReal = new File(saveDir, this.getFileName());
                FileOutputStream os = new FileOutputStream(fileTmp);
                NbtIo.writeCompressed(this.writeToNBT(new CompoundTag()), os);
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
