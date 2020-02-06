package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;

@Mixin(net.minecraft.client.gui.screen.ingame.ContainerScreen.class)
public interface IMixinContainerScreen
{
    @Invoker("getSlotAt")
    net.minecraft.container.Slot getSlotAtPositionInvoker(double x, double y);

    @Invoker("onMouseClick")
    void handleMouseClickInvoker(net.minecraft.container.Slot slotIn, int slotId, int mouseButton, net.minecraft.container.SlotActionType type);

    @Accessor("focusedSlot")
    net.minecraft.container.Slot getHoveredSlot();

    @Accessor("x")
    int getGuiLeft();

    @Accessor("y")
    int getGuiTop();

    @Accessor("containerWidth")
    int getGuiSizeX();

    @Accessor("containerHeight")
    int getGuiSizeY();
}
