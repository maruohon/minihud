package fi.dy.masa.itemscroller.util;

import fi.dy.masa.itemscroller.mixin.IMixinContainerScreen;
import fi.dy.masa.itemscroller.mixin.IMixinVillagerScreen;
import fi.dy.masa.itemscroller.mixin.IMixinSlot;
import net.minecraft.client.gui.ContainerScreen;
import net.minecraft.client.gui.container.VillagerScreen;
import net.minecraft.container.Slot;
import net.minecraft.container.SlotActionType;

public class AccessorUtils
{
    public static Slot getSlotUnderMouse(ContainerScreen<?> gui)
    {
        return ((IMixinContainerScreen) gui).getHoveredSlot();
    }

    public static Slot getSlotAtPosition(ContainerScreen<?> gui, int x, int y)
    {
        return ((IMixinContainerScreen) gui).getSlotAtPositionInvoker(x, y);
    }

    public static void handleMouseClick(ContainerScreen<?> gui, Slot slotIn, int slotId, int mouseButton, SlotActionType type)
    {
        ((IMixinContainerScreen) gui).handleMouseClickInvoker(slotIn, slotId, mouseButton, type);
    }

    public static int getGuiLeft(ContainerScreen<?> gui)
    {
        return ((IMixinContainerScreen) gui).getGuiLeft();
    }

    public static int getGuiTop(ContainerScreen<?> gui)
    {
        return ((IMixinContainerScreen) gui).getGuiTop();
    }

    public static int getGuiXSize(ContainerScreen<?> gui)
    {
        return ((IMixinContainerScreen) gui).getGuiSizeX();
    }

    public static int getGuiYSize(ContainerScreen<?> gui)
    {
        return ((IMixinContainerScreen) gui).getGuiSizeY();
    }

    public static int getSelectedMerchantRecipe(VillagerScreen gui)
    {
        return ((IMixinVillagerScreen) gui).getSelectedMerchantRecipe();
    }

    public static int getSlotIndex(Slot slot)
    {
        return ((IMixinSlot) slot).getSlotIndex();
    }
}
