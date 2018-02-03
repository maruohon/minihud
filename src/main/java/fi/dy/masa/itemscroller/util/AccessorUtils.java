package fi.dy.masa.itemscroller.util;

import fi.dy.masa.itemscroller.interfaces.IGuiContainerAccessor;
import fi.dy.masa.itemscroller.interfaces.IGuiMerchantAccessor;
import fi.dy.masa.itemscroller.interfaces.ISlotAccessor;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Slot;

public class AccessorUtils
{
    public static Slot getSlotUnderMouse(GuiContainer gui)
    {
        return ((IGuiContainerAccessor) gui).getHoveredSlot();
    }

    public static Slot getSlotAtPosition(GuiContainer gui, int x, int y)
    {
        return ((IGuiContainerAccessor) gui).getSlotAt(x, y);
    }

    public static int getGuiLeft(GuiContainer gui)
    {
        return ((IGuiContainerAccessor) gui).getGuiLeft();
    }

    public static int getGuiTop(GuiContainer gui)
    {
        return ((IGuiContainerAccessor) gui).getGuiTop();
    }

    public static int getGuiXSize(GuiContainer gui)
    {
        return ((IGuiContainerAccessor) gui).getGuiSizeX();
    }

    public static int getGuiYSize(GuiContainer gui)
    {
        return ((IGuiContainerAccessor) gui).getGuiSizeY();
    }

    public static int getSelectedMerchantRecipe(GuiMerchant gui)
    {
        return ((IGuiMerchantAccessor) gui).getSelectedMerchantRecipe();
    }

    public static int getSlotIndex(Slot slot)
    {
        return ((ISlotAccessor) slot).getSlotIndex();
    }
}
