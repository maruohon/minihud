package fi.dy.masa.itemscroller.villager;

import it.unimi.dsi.fastutil.ints.IntArrayList;

public class FavoriteData
{
    public final IntArrayList favorites;
    public final boolean isGlobal;

    public FavoriteData(IntArrayList favorites, boolean isGlobal)
    {
        this.favorites = favorites;
        this.isGlobal = isGlobal;
    }
}
