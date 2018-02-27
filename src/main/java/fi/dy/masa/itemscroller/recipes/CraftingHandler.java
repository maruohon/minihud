package fi.dy.masa.itemscroller.recipes;

import java.util.HashMap;
import java.util.Map;
import javax.annotation.Nullable;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.client.gui.inventory.GuiCrafting;
import net.minecraft.client.gui.inventory.GuiInventory;
import net.minecraft.inventory.Slot;
import net.minecraft.inventory.SlotCrafting;

public class CraftingHandler
{
    private static final Map<CraftingOutputSlot, SlotRange> CRAFTING_GRID_SLOTS = new HashMap<CraftingOutputSlot, SlotRange>();

    static
    {
        //"net.minecraft.client.gui.inventory.GuiCrafting,net.minecraft.inventory.SlotCrafting,0,1-9", // vanilla Crafting Table
        CRAFTING_GRID_SLOTS.put(new CraftingOutputSlot(GuiCrafting.class, SlotCrafting.class, 0), new SlotRange(1, 9));
        //"net.minecraft.client.gui.inventory.GuiInventory,net.minecraft.inventory.SlotCrafting,0,1-4", // vanilla player inventory crafting grid
        CRAFTING_GRID_SLOTS.put(new CraftingOutputSlot(GuiInventory.class, SlotCrafting.class, 0), new SlotRange(1, 4));
    }

    /**
     * Gets the crafting grid SlotRange associated with the given slot in the given gui, if any.
     * @param gui
     * @param slot
     * @return the SlotRange of the crafting grid, or null, if the given slot is not a crafting output slot
     */
    @Nullable
    public static SlotRange getCraftingGridSlots(GuiContainer gui, Slot slot)
    {
        for (Map.Entry<CraftingOutputSlot, SlotRange> entry : CRAFTING_GRID_SLOTS.entrySet())
        {
            if (entry.getKey().matches(gui.getClass(), slot.getClass(), slot.slotNumber))
            {
                return entry.getValue();
            }
        }

        return null;
    }

    @Nullable
    public static Slot getFirstCraftingOutputSlotForGui(GuiContainer gui)
    {
        for (Slot slot : gui.inventorySlots.inventorySlots)
        {
            if (getCraftingGridSlots(gui, slot) != null)
            {
                return slot;
            }
        }

        return null;
    }

    public static class CraftingOutputSlot
    {
        private final Class<? extends GuiContainer> guiClass;
        private final Class<? extends Slot> slotClass;
        private final int outputSlot;

        private CraftingOutputSlot (Class<? extends GuiContainer> guiClass, Class<? extends Slot> slotClass, int outputSlot)
        {
            this.guiClass = guiClass;
            this.slotClass = slotClass;
            this.outputSlot = outputSlot;
        }

        public Class<? extends GuiContainer> getGuiClass()
        {
            return this.guiClass;
        }

        public Class<? extends Slot> getSlotClass()
        {
            return this.slotClass;
        }

        public int getSlotNumber()
        {
            return this.outputSlot;
        }

        public boolean matches(Class<? extends GuiContainer> guiClass, Class<? extends Slot> slotClass, int outputSlot)
        {
            return outputSlot == this.outputSlot && this.guiClass == guiClass && this.slotClass == slotClass;
        }

        @Override
        public int hashCode()
        {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((guiClass == null) ? 0 : guiClass.hashCode());
            result = prime * result + outputSlot;
            result = prime * result + ((slotClass == null) ? 0 : slotClass.hashCode());
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
            CraftingOutputSlot other = (CraftingOutputSlot) obj;
            if (guiClass == null)
            {
                if (other.guiClass != null)
                    return false;
            }
            else if (guiClass != other.guiClass)
                return false;
            if (outputSlot != other.outputSlot)
                return false;
            if (slotClass == null)
            {
                if (other.slotClass != null)
                    return false;
            }
            else if (slotClass != other.slotClass)
                return false;
            return true;
        }

    }

    public static class SlotRange
    {
        private final int first;
        private final int last;

        public SlotRange(int start, int numSlots)
        {
            this.first = start;
            this.last = start + numSlots - 1;
        }

        public int getFirst()
        {
            return this.first;
        }

        public int getLast()
        {
            return this.last;
        }

        public int getSlotCount()
        {
            return this.last - this.first + 1;
        }

        public boolean contains(int slot)
        {
            return slot >= this.first && slot <= this.last;
        }

        @Override
        public String toString()
        {
            return String.format("SlotRange: {first: %d, last: %d}", this.first, this.last);
        }
    }
}
