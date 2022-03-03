package fi.dy.masa.itemscroller.util;

import javax.annotation.Nullable;
import net.minecraft.client.gui.screen.ingame.HandledScreen;
import net.minecraft.client.gui.screen.ingame.MerchantScreen;
import net.minecraft.screen.slot.Slot;
import net.minecraft.screen.slot.SlotActionType;
import fi.dy.masa.itemscroller.mixin.IMixinMerchantScreen;
import fi.dy.masa.itemscroller.mixin.IMixinScreenWithHandler;
import fi.dy.masa.itemscroller.mixin.IMixinSlot;

public class AccessorUtils
{
    @Nullable
    public static Slot getSlotUnderMouse(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).itemscroller_getHoveredSlot();
    }

    @Nullable
    public static Slot getSlotAtPosition(HandledScreen<?> gui, int x, int y)
    {
        return ((IMixinScreenWithHandler) gui).itemscroller_getSlotAtPositionInvoker(x, y);
    }

    public static void handleMouseClick(HandledScreen<?> gui, Slot slotIn, int slotId, int mouseButton, SlotActionType type)
    {
        ((IMixinScreenWithHandler) gui).itemscroller_handleMouseClickInvoker(slotIn, slotId, mouseButton, type);
    }

    public static int getGuiLeft(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).itemscroller_getGuiLeft();
    }

    public static int getGuiTop(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).itemscroller_getGuiTop();
    }

    public static int getGuiXSize(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).itemscroller_getBackgroundWidth();
    }

    public static int getGuiYSize(HandledScreen<?> gui)
    {
        return ((IMixinScreenWithHandler) gui).itemscroller_getBackgroundHeight();
    }

    public static int getSelectedMerchantRecipe(MerchantScreen gui)
    {
        return ((IMixinMerchantScreen) gui).itemscroller_getSelectedMerchantRecipe();
    }

    public static int getSlotIndex(Slot slot)
    {
        return ((IMixinSlot) slot).itemscroller_getSlotIndex();
    }
}
