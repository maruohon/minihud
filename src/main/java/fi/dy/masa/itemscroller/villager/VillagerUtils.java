package fi.dy.masa.itemscroller.villager;

import java.util.List;
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
        VillagerData data = VillagerDataStorage.getInstance().getDataForLastInteractionTarget();

        if (data != null)
        {
            TradeOfferList list = new TradeOfferList();
            List<Integer> favorites = data.getFavorites();
            int originalListSize = originalList.size();

            // Some favorites defined
            if (favorites.isEmpty() == false)
            {
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
            }
            else
            {
                list.addAll(originalList);
            }

            return list;
        }

        return originalList;
    }
}
