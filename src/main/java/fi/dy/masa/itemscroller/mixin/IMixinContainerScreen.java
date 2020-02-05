package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.client.gui.screen.ingame.ContainerScreen;
import net.minecraft.container.Slot;
import net.minecraft.container.SlotActionType;

@Mixin(ContainerScreen.class)
public interface IMixinContainerScreen
{
    @Invoker("getSlotAt")
    Slot getSlotAtPositionInvoker(double x, double y);

    @Invoker("onMouseClick")
    void handleMouseClickInvoker(Slot slotIn, int slotId, int mouseButton, SlotActionType type);

    @Accessor("focusedSlot")
    Slot getHoveredSlot();

    @Accessor("x")
    int getGuiLeft();

    @Accessor("y")
    int getGuiTop();

    @Accessor("containerWidth")
    int getGuiSizeX();

    @Accessor("containerHeight")
    int getGuiSizeY();
}
