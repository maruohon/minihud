package fi.dy.masa.itemscroller.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.annotation.Nonnull;
import net.minecraft.item.ItemStack;

/**
 * Wrapper class for ItemStack, which implements equals()
 * for the item, damage and NBT, but not stackSize.
 */
public class ItemType
{
    private final ItemStack stack;

    public ItemType(@Nonnull ItemStack stack)
    {
        this.stack = stack.copy();
    }

    public ItemStack getStack()
    {
        return this.stack;
    }

    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        //result = prime * result + ((stack == null) ? 0 : stack.hashCode());
        result = prime * result + this.stack.getMetadata();
        result = prime * result + this.stack.getItem().hashCode();
        result = prime * result + (this.stack.getTagCompound() != null ? this.stack.getTagCompound().hashCode() : 0);
        return result;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;

        ItemType other = (ItemType) obj;

        if (InventoryUtils.isStackEmpty(this.stack) || InventoryUtils.isStackEmpty(other.stack))
        {
            if (InventoryUtils.isStackEmpty(this.stack) != InventoryUtils.isStackEmpty(other.stack))
                return false;
        }
        else
        {
            if (this.stack.getMetadata() != other.stack.getMetadata())
            {
                return false;
            }

            if (this.stack.getItem() != other.stack.getItem())
            {
                return false;
            }

            return ItemStack.areItemStackTagsEqual(this.stack, other.stack);
        }

        return true;
    }

    /**
     * Returns a map that has a list of the indices for each different item in the input list
     * @param stacks
     * @return
     */
    public static Map<ItemType, List<Integer>> getSlotsPerItem(ItemStack[] stacks)
    {
        Map<ItemType, List<Integer>> mapSlots = new HashMap<ItemType, List<Integer>>();

        for (int i = 0; i < stacks.length; i++)
        {
            ItemStack stack = stacks[i];

            if (InventoryUtils.isStackEmpty(stack) == false)
            {
                ItemType item = new ItemType(stack);
                List<Integer> slots = mapSlots.get(item);

                if (slots == null)
                {
                    slots = new ArrayList<Integer>();
                    mapSlots.put(item, slots);
                }

                slots.add(i);
            }
        }

        return mapSlots;
    }
}
