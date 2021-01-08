package fi.dy.masa.itemscroller.villager;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.gui.screen.ingame.MerchantScreen;
import net.minecraft.network.packet.c2s.play.SelectMerchantTradeC2SPacket;
import net.minecraft.screen.MerchantScreenHandler;
import net.minecraft.village.TradeOffer;
import net.minecraft.village.TradeOfferList;
import fi.dy.masa.malilib.util.GuiUtils;

public class VillagerUtils
{
    public static boolean switchToTradeByVisibleIndex(int visibleIndex)
    {
        Screen screen = GuiUtils.getCurrentScreen();

        if (screen instanceof MerchantScreen)
        {
            MerchantScreen merchantScreen = (MerchantScreen) screen;
            MerchantScreenHandler handler = merchantScreen.getScreenHandler();

            int realIndex = getRealTradeIndexFor(visibleIndex, handler);

            if (realIndex >= 0)
            {
                // Use the real (server-side) index
                handler.setRecipeIndex(realIndex);

                // Use the "visible index", since this will access the custom list
                handler.switchTo(visibleIndex);

                // Use the real (server-side) index
                MinecraftClient.getInstance().getNetworkHandler().sendPacket(new SelectMerchantTradeC2SPacket(realIndex));

                return true;
            }
        }

        return false;
    }

    public static int getRealTradeIndexFor(int visibleIndex, MerchantScreenHandler handler)
    {
        if (handler instanceof IMerchantScreenHandler)
        {
            TradeOfferList originalList = ((IMerchantScreenHandler) handler).getOriginalList();
            TradeOfferList customList = handler.getRecipes();

            if (originalList != null && customList != null &&
                visibleIndex >= 0 && visibleIndex < customList.size())
            {
                TradeOffer trade = customList.get(visibleIndex);

                if (trade != null)
                {
                    int realIndex = originalList.indexOf(trade);

                    if (realIndex >= 0 && realIndex < originalList.size())
                    {
                        return realIndex;
                    }
                }
            }
        }

        return -1;
    }

    public static TradeOfferList buildCustomTradeList(TradeOfferList originalList)
    {
        FavoriteData data = VillagerDataStorage.getInstance().getFavoritesForCurrentVillager(originalList);
        List<Integer> favorites = data.favorites;

        //System.out.printf("build - fav: %s (%s), or: %d\n", favorites, data.isGlobal, originalList.size());

        // Some favorites defined
        if (favorites.isEmpty() == false)
        {
            TradeOfferList list = new TradeOfferList();
            int originalListSize = originalList.size();

            // First pick all the favorited recipes, in the order they are in the favorites list
            for (int index : favorites)
            {
                if (index >= 0 && index < originalListSize)
                {
                    list.add(originalList.get(index));
                }
            }

            // Then add the rest of the recipes in their original order
            for (int i = 0; i < originalListSize; ++i)
            {
                if (favorites.contains(i) == false)
                {
                    list.add(originalList.get(i));
                }
            }

            return list;
        }

        return originalList;
    }

    public static List<Integer> getGlobalFavoritesFor(TradeOfferList originalTrades, Collection<TradeType> globalFavorites)
    {
        List<Integer> favorites = new ArrayList<>();
        Map<TradeType, Integer> trades = new HashMap<>();
        final int size = originalTrades.size();

        // Build a map from the trade types to the indices in the current villager's trade list
        for (int i = 0; i < size; ++i)
        {
            TradeOffer trade = originalTrades.get(i);
            trades.put(TradeType.of(trade), i);
        }

        // Pick the trade list indices that are in the global favorites, in the order that they were global favorited
        for (TradeType type : globalFavorites)
        {
            Integer index = trades.get(type);

            if (index != null)
            {
                favorites.add(index);
            }
        }

        /* This is a version that is not sorted based on the order of the global favorites
        for (int i = 0; i < size; ++i)
        {
            TradeType type = TradeType.of(originalTrades.get(i));

            if (globalFavorites.contains(type))
            {
                favorites.add(i);
            }
        }
        */
        //System.out.printf("getGlobalFavoritesFor - list: %s - or: %d | global: %s\n", favorites, originalTrades.size(), globalFavorites);

        return favorites;
    }
}
