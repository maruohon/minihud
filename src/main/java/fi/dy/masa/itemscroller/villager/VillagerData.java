package fi.dy.masa.itemscroller.villager;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.IntTag;
import net.minecraft.nbt.ListTag;
import fi.dy.masa.itemscroller.util.Constants;

public class VillagerData
{
    private final UUID uuid;
    private List<Integer> favorites = new ArrayList<>();
    private int tradeListPosition;

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

    List<Integer> getFavorites()
    {
        return this.favorites;
    }

    public CompoundTag toNBT()
    {
        CompoundTag tag = new CompoundTag();

        tag.putLong("UUIDM", this.uuid.getMostSignificantBits());
        tag.putLong("UUIDL", this.uuid.getLeastSignificantBits());
        tag.putInt("ListPosition", this.tradeListPosition);

        ListTag tagList = new ListTag();

        for (Integer val : this.favorites)
        {
            tagList.add(IntTag.of(val));
        }

        tag.put("Favorites", tagList);

        return tag;
    }

    @Nullable
    public static VillagerData fromNBT(CompoundTag tag)
    {
        if (tag.contains("UUIDM", Constants.NBT.TAG_LONG) && tag.contains("UUIDL", Constants.NBT.TAG_LONG))
        {
            VillagerData data = new VillagerData(new UUID(tag.getLong("UUIDM"), tag.getLong("UUIDL")));
            ListTag tagList = tag.getList("Favorites", Constants.NBT.TAG_INT);
            final int count = tagList.size();

            data.favorites.clear();
            data.tradeListPosition = tag.getInt("ListPosition");

            for (int i = 0; i < count; ++i)
            {
                data.favorites.add(tagList.getInt(i));
            }

            return data;
        }

        return null;
    }
}
