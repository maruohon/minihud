package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.screen.slot.Slot;

@Mixin(net.minecraft.client.gui.screen.ingame.HandledScreen.class)
public interface IMixinScreenWithHandler
{
    @Invoker("getSlotAt")
    Slot itemscroller_getSlotAtPositionInvoker(double x, double y);

    @Invoker("onMouseClick")
    void itemscroller_handleMouseClickInvoker(Slot slotIn, int slotId, int mouseButton, net.minecraft.screen.slot.SlotActionType type);

    @Accessor("focusedSlot")
    Slot itemscroller_getHoveredSlot();

    @Accessor("x")
    int itemscroller_getGuiLeft();

    @Accessor("y")
    int itemscroller_getGuiTop();

    @Accessor("backgroundWidth")
    int itemscroller_getBackgroundWidth();

    @Accessor("backgroundHeight")
    int itemscroller_getBackgroundHeight();
}
