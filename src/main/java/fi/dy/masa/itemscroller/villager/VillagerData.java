package fi.dy.masa.itemscroller.villager;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import javax.annotation.Nullable;
import fi.dy.masa.itemscroller.util.Constants;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagInt;
import net.minecraft.nbt.NBTTagList;

public class VillagerData
{
    private final UUID uuid;
    private List<Integer> favorites = new ArrayList<>();
    private int tradeListPosition;
    private int lastPage;

    VillagerData(UUID uuid)
    {
        this.uuid = uuid;
    }

    public UUID getUUID()
    {
        return this.uuid;
    }

    public int getTradeListPosition()
    {
        return this.tradeListPosition;
    }

    void setTradeListPosition(int position)
    {
        this.tradeListPosition = position;
    }

    public int getLastPage()
    {
        return this.lastPage;
    }

    void setLastPage(int page)
    {
        this.lastPage = page;
    }

    void toggleFavorite(int tradeIndex)
    {
        if (this.favorites.contains(tradeIndex))
        {
            this.favorites.remove(Integer.valueOf(tradeIndex));
        }
        else
        {
            this.favorites.add(tradeIndex);
        }
    }

    public List<Integer> getFavorites()
    {
        return this.favorites;
    }

    public NBTTagCompound toNBT()
    {
        NBTTagCompound tag = new NBTTagCompound();

        tag.setLong("UUIDM", this.uuid.getMostSignificantBits());
        tag.setLong("UUIDL", this.uuid.getLeastSignificantBits());
        tag.setInteger("ListPosition", this.tradeListPosition);
        tag.setInteger("CurrentPage", this.lastPage);

        NBTTagList tagList = new NBTTagList();

        for (Integer val : this.favorites)
        {
            tagList.appendTag(new NBTTagInt(val.intValue()));
        }

        tag.setTag("Favorites", tagList);

        return tag;
    }

    @Nullable
    public static VillagerData fromNBT(NBTTagCompound tag)
    {
        if (tag.hasKey("UUIDM", Constants.NBT.TAG_LONG) && tag.hasKey("UUIDL", Constants.NBT.TAG_LONG))
        {
            VillagerData data = new VillagerData(new UUID(tag.getLong("UUIDM"), tag.getLong("UUIDL")));

            data.tradeListPosition = tag.getInteger("ListPosition");
            data.lastPage = tag.getInteger("CurrentPage");
            NBTTagList tagList = tag.getTagList("Favorites", Constants.NBT.TAG_INT);

            final int count = tagList.tagCount();
            data.favorites.clear();

            for (int i = 0; i < count; ++i)
            {
                data.favorites.add(tagList.getIntAt(i));
            }

            return data;
        }

        return null;
    }
}
