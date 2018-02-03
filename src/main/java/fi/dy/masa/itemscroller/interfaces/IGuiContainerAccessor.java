package fi.dy.masa.itemscroller.interfaces;

import net.minecraft.inventory.Slot;

public interface IGuiContainerAccessor
{
    Slot getSlotAt(int x, int y);

    Slot getHoveredSlot();

    int getGuiLeft();

    int getGuiTop();

    int getGuiSizeX();

    int getGuiSizeY();
}
