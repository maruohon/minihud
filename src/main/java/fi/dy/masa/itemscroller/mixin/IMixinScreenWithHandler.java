package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;

@Mixin(net.minecraft.client.gui.screen.ingame.HandledScreen.class)
public interface IMixinScreenWithHandler
{
    @Invoker("getSlotAt")
    net.minecraft.screen.slot.Slot getSlotAtPositionInvoker(double x, double y);

    @Invoker("onMouseClick")
    void handleMouseClickInvoker(net.minecraft.screen.slot.Slot slotIn, int slotId, int mouseButton, net.minecraft.screen.slot.SlotActionType type);

    @Accessor("focusedSlot")
    net.minecraft.screen.slot.Slot getHoveredSlot();

    @Accessor("field_2776") // x
    int getGuiLeft();

    @Accessor("field_2800") // y
    int getGuiTop();

    @Accessor("backgroundWidth")
    int getGuiSizeX();

    @Accessor("backgroundHeight")
    int getGuiSizeY();
}
