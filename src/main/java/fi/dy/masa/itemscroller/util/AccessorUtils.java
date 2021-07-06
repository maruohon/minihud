package fi.dy.masa.itemscroller.util;

import net.minecraft.client.gui.screen.ingame.HandledScreen;
import net.minecraft.client.gui.screen.ingame.MerchantScreen;
import net.minecraft.screen.slot.Slot;
import net.minecraft.screen.slot.SlotActionType;
import fi.dy.masa.itemscroller.mixin.IMixinMerchantScreen;
import fi.dy.masa.itemscroller.mixin.IMixinScreenWithHandler;
import fi.dy.masa.itemscroller.mixin.IMixinSlot;

public class AccessorUtils
{
    public static Slot getSlotUnderMouse(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).getHoveredSlot();
    }

    public static Slot getSlotAtPosition(HandledScreen<?> gui, int x, int y)
    {
        return ((IMixinScreenWithHandler) gui).getSlotAtPositionInvoker(x, y);
    }

    public static void handleMouseClick(HandledScreen<?> gui, Slot slotIn, int slotId, int mouseButton, SlotActionType type)
    {
        ((IMixinScreenWithHandler) gui).handleMouseClickInvoker(slotIn, slotId, mouseButton, type);
    }

    public static int getGuiLeft(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).getGuiLeft();
    }

    public static int getGuiTop(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).getGuiTop();
    }

    public static int getGuiXSize(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).getGuiSizeX();
    }

    public static int getGuiYSize(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).getGuiSizeY();
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
