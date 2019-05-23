package fi.dy.masa.itemscroller.util;

import fi.dy.masa.itemscroller.mixin.IMixinAbstractContainerScreen;
import fi.dy.masa.itemscroller.mixin.IMixinSlot;
import fi.dy.masa.itemscroller.mixin.IMixinMerchantScreen;
import net.minecraft.client.gui.screen.ingame.AbstractContainerScreen;
import net.minecraft.client.gui.screen.ingame.MerchantScreen;
import net.minecraft.container.Slot;
import net.minecraft.container.SlotActionType;

public class AccessorUtils
{
    public static Slot getSlotUnderMouse(AbstractContainerScreen<?> gui)
    {
        return ((IMixinAbstractContainerScreen) gui).getHoveredSlot();
    }

    public static Slot getSlotAtPosition(AbstractContainerScreen<?> gui, int x, int y)
    {
        return ((IMixinAbstractContainerScreen) gui).getSlotAtPositionInvoker(x, y);
    }

    public static void handleMouseClick(AbstractContainerScreen<?> gui, Slot slotIn, int slotId, int mouseButton, SlotActionType type)
    {
        ((IMixinAbstractContainerScreen) gui).handleMouseClickInvoker(slotIn, slotId, mouseButton, type);
    }

    public static int getGuiLeft(AbstractContainerScreen<?> gui)
    {
        return ((IMixinAbstractContainerScreen) gui).getGuiLeft();
    }

    public static int getGuiTop(AbstractContainerScreen<?> gui)
    {
        return ((IMixinAbstractContainerScreen) gui).getGuiTop();
    }

    public static int getGuiXSize(AbstractContainerScreen<?> gui)
    {
        return ((IMixinAbstractContainerScreen) gui).getGuiSizeX();
    }

    public static int getGuiYSize(AbstractContainerScreen<?> gui)
    {
        return ((IMixinAbstractContainerScreen) gui).getGuiSizeY();
    }

    public static int getSelectedMerchantRecipe(MerchantScreen gui)
    {
        return ((IMixinMerchantScreen) gui).getSelectedMerchantRecipe();
    }

    public static int getSlotIndex(Slot slot)
    {
        return ((IMixinSlot) slot).getSlotIndex();
    }
}
