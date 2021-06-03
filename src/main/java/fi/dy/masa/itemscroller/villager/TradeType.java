package fi.dy.masa.itemscroller.villager;

import javax.annotation.Nullable;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.util.Identifier;
import net.minecraft.util.registry.Registry;
import net.minecraft.village.TradeOffer;

public class TradeType
{
    public final Item buyItem1;
    public final Item buyItem2;
    public final Item sellItem;

    public TradeType(Item buyItem1, Item buyItem2, Item sellItem)
    {
        this.buyItem1 = buyItem1;
        this.buyItem2 = buyItem2;
        this.sellItem = sellItem;
    }

    public boolean matchesTrade(TradeOffer trade)
    {
        ItemStack stackBuyItem1 = trade.getOriginalFirstBuyItem();
        ItemStack stackBuyItem2 = trade.getSecondBuyItem();
        ItemStack stackSellItem = trade.getSellItem();
        Item buyItem1 = stackBuyItem1.getItem();
        Item buyItem2 = stackBuyItem2.getItem();
        Item sellItem = stackSellItem.getItem();

        return this.buyItem1 == buyItem1 && this.buyItem2 == buyItem2 && this.sellItem == sellItem;
    }

    public NbtCompound toTag()
    {
        NbtCompound tag = new NbtCompound();

        tag.putString("Buy1", Registry.ITEM.getId(this.buyItem1).toString());
        tag.putString("Buy2", Registry.ITEM.getId(this.buyItem2).toString());
        tag.putString("Sell", Registry.ITEM.getId(this.sellItem).toString());

        return tag;
    }

    @Nullable
    public static TradeType fromTag(NbtCompound tag)
    {
        Item buy1 = getItemForName(tag.getString("Buy1"));
        Item buy2 = getItemForName(tag.getString("Buy2"));
        Item sell = getItemForName(tag.getString("Sell"));

        if (buy1 != Items.AIR || buy2 != Items.AIR || sell != Items.AIR)
        {
            return new TradeType(buy1, buy2, sell);
        }

        return null;
    }

    public static Item getItemForName(String name)
    {
        try
        {
            Identifier id = new Identifier(name);
            return Registry.ITEM.get(id);
        }
        catch (Exception e)
        {
            return Items.AIR;
        }
    }

    @Override
    public boolean equals(Object o)
    {
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }

        TradeType tradeType = (TradeType) o;

        if (!buyItem1.equals(tradeType.buyItem1)) { return false; }
        if (!buyItem2.equals(tradeType.buyItem2)) { return false; }
        return sellItem.equals(tradeType.sellItem);
    }

    @Override
    public int hashCode()
    {
        int result = buyItem1.hashCode();
        result = 31 * result + buyItem2.hashCode();
        result = 31 * result + sellItem.hashCode();
        return result;
    }

    public static TradeType of(TradeOffer trade)
    {
        ItemStack stackBuyItem1 = trade.getOriginalFirstBuyItem();
        ItemStack stackBuyItem2 = trade.getSecondBuyItem();
        ItemStack stackSellItem = trade.getSellItem();
        Item buyItem1 = stackBuyItem1.getItem();
        Item buyItem2 = stackBuyItem2.getItem();
        Item sellItem = stackSellItem.getItem();

        return new TradeType(buyItem1, buyItem2, sellItem);
    }
}
