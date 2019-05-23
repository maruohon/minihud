package fi.dy.masa.itemscroller.recipes;

import javax.annotation.Nonnull;
import fi.dy.masa.itemscroller.recipes.CraftingHandler.SlotRange;
import fi.dy.masa.itemscroller.util.Constants;
import fi.dy.masa.itemscroller.util.InventoryUtils;
import net.minecraft.client.gui.screen.ingame.AbstractContainerScreen;
import net.minecraft.container.Container;
import net.minecraft.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

public class RecipePattern
{
    private ItemStack result = InventoryUtils.EMPTY_STACK;
    private ItemStack[] recipe = new ItemStack[9];

    public RecipePattern()
    {
        this.ensureRecipeSizeAndClearRecipe(9);
    }

    public void ensureRecipeSize(int size)
    {
        if (this.getRecipeLength() != size)
        {
            this.recipe = new ItemStack[size];
        }
    }

    public void clearRecipe()
    {
        for (int i = 0; i < this.recipe.length; i++)
        {
            this.recipe[i] = InventoryUtils.EMPTY_STACK;
        }

        this.result = InventoryUtils.EMPTY_STACK;
    }

    public void ensureRecipeSizeAndClearRecipe(int size)
    {
        this.ensureRecipeSize(size);
        this.clearRecipe();
    }

    public void storeCraftingRecipe(Slot slot, AbstractContainerScreen<? extends Container> gui, boolean clearIfEmpty)
    {
        SlotRange range = CraftingHandler.getCraftingGridSlots(gui, slot);

        if (range != null)
        {
            if (slot.hasStack())
            {
                if (InventoryUtils.areStacksEqual(this.getResult(), slot.getStack()) == false ||
                    this.getRecipeLength() != range.getSlotCount())
                {
                    int gridSize = range.getSlotCount();
                    int numSlots = gui.getContainer().slotList.size();

                    this.ensureRecipeSizeAndClearRecipe(gridSize);

                    for (int i = 0, s = range.getFirst(); i < gridSize && s < numSlots; i++, s++)
                    {
                        Slot slotTmp = gui.getContainer().getSlot(s);
                        this.recipe[i] = slotTmp.hasStack() ? slotTmp.getStack().copy() : InventoryUtils.EMPTY_STACK;
                    }

                    this.result = slot.getStack().copy();
                }
            }
            else if (clearIfEmpty)
            {
                this.clearRecipe();
            }
        }
    }

    public void copyRecipeFrom(RecipePattern other)
    {
        int size = other.getRecipeLength();
        ItemStack[] otherRecipe = other.getRecipeItems();

        this.ensureRecipeSizeAndClearRecipe(size);

        for (int i = 0; i < size; i++)
        {
            this.recipe[i] = InventoryUtils.isStackEmpty(otherRecipe[i]) == false ? otherRecipe[i].copy() : InventoryUtils.EMPTY_STACK;
        }

        this.result = InventoryUtils.isStackEmpty(other.getResult()) == false ? other.getResult().copy() : InventoryUtils.EMPTY_STACK;
    }

    public void readFromNBT(@Nonnull CompoundTag nbt)
    {
        if (nbt.containsKey("Result", Constants.NBT.TAG_COMPOUND) && nbt.containsKey("Ingredients", Constants.NBT.TAG_LIST))
        {
            ListTag tagIngredients = nbt.getList("Ingredients", Constants.NBT.TAG_COMPOUND);
            int count = tagIngredients.size();
            int length = nbt.getInt("Length");

            if (length > 0)
            {
                this.ensureRecipeSizeAndClearRecipe(length);
            }

            for (int i = 0; i < count; i++)
            {
                CompoundTag tag = tagIngredients.getCompoundTag(i);
                int slot = tag.getInt("Slot");

                if (slot >= 0 && slot < this.recipe.length)
                {
                    this.recipe[slot] = ItemStack.fromTag(tag);
                }
            }

            this.result = ItemStack.fromTag(nbt.getCompound("Result"));
        }
    }

    @Nonnull
    public CompoundTag writeToNBT(@Nonnull CompoundTag nbt)
    {
        if (this.isValid())
        {
            CompoundTag tag = new CompoundTag();
            this.result.toTag(tag);

            nbt.putInt("Length", this.recipe.length);
            nbt.put("Result", tag);

            ListTag tagIngredients = new ListTag();

            for (int i = 0; i < this.recipe.length; i++)
            {
                if (InventoryUtils.isStackEmpty(this.recipe[i]) == false)
                {
                    tag = new CompoundTag();
                    tag.putInt("Slot", i);
                    this.recipe[i].toTag(tag);
                    tagIngredients.add(tag);
                }
            }

            nbt.put("Ingredients", tagIngredients);
        }

        return nbt;
    }

    public ItemStack getResult()
    {
        return this.result;
    }

    public int getRecipeLength()
    {
        return this.recipe.length;
    }

    public ItemStack[] getRecipeItems()
    {
        return this.recipe;
    }

    public boolean isValid()
    {
        return InventoryUtils.isStackEmpty(this.getResult()) == false;
    }
}
