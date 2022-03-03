package fi.dy.masa.itemscroller.villager;

import java.util.UUID;
import javax.annotation.Nullable;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtInt;
import net.minecraft.nbt.NbtList;
import fi.dy.masa.itemscroller.util.Constants;
import it.unimi.dsi.fastutil.ints.IntArrayList;

public class VillagerData
{
    private final UUID uuid;
    private final IntArrayList favorites = new IntArrayList();
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
            this.favorites.rem(tradeIndex);
        }
        else
        {
            this.favorites.add(tradeIndex);
        }
    }

    IntArrayList getFavorites()
    {
        return this.favorites;
    }

    public NbtCompound toNBT()
    {
        NbtCompound tag = new NbtCompound();

        tag.putLong("UUIDM", this.uuid.getMostSignificantBits());
        tag.putLong("UUIDL", this.uuid.getLeastSignificantBits());
        tag.putInt("ListPosition", this.tradeListPosition);

        NbtList tagList = new NbtList();

        for (Integer val : this.favorites)
        {
            tagList.add(NbtInt.of(val));
        }

        tag.put("Favorites", tagList);

        return tag;
    }

    @Nullable
    public static VillagerData fromNBT(NbtCompound tag)
    {
        if (tag.contains("UUIDM", Constants.NBT.TAG_LONG) && tag.contains("UUIDL", Constants.NBT.TAG_LONG))
        {
            VillagerData data = new VillagerData(new UUID(tag.getLong("UUIDM"), tag.getLong("UUIDL")));
            NbtList tagList = tag.getList("Favorites", Constants.NBT.TAG_INT);
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
