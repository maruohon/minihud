package fi.dy.masa.itemscroller.villager;

import java.util.List;

public class FavoriteData
{
    public final List<Integer> favorites;
    public final boolean isGlobal;

    public FavoriteData(List<Integer> favorites, boolean isGlobal)
    {
        this.favorites = favorites;
        this.isGlobal = isGlobal;
    }
}
