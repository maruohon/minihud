package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.client.gui.screen.ingame.AbstractContainerScreen;
import net.minecraft.container.Slot;
import net.minecraft.container.SlotActionType;

@Mixin(AbstractContainerScreen.class)
public interface IMixinAbstractContainerScreen
{
    @Invoker("getSlotAt")
    Slot getSlotAtPositionInvoker(double x, double y);

    @Invoker("onMouseClick")
    void handleMouseClickInvoker(Slot slotIn, int slotId, int mouseButton, SlotActionType type);

    @Accessor("focusedSlot")
    Slot getHoveredSlot();

    @Accessor("left")
    int getGuiLeft();

    @Accessor("top")
    int getGuiTop();

    @Accessor("containerWidth")
    int getGuiSizeX();

    @Accessor("containerHeight")
    int getGuiSizeY();
}
