package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import org.spongepowered.asm.mixin.gen.Invoker;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.ClickType;
import net.minecraft.inventory.Slot;

@Mixin(GuiContainer.class)
public interface IMixinGuiContainer
{
    @Invoker("getSlotAtPosition")
    Slot getSlotAtPositionInvoker(int x, int y);

    @Invoker("handleMouseClick")
    void handleMouseClickInvoker(Slot slotIn, int slotId, int mouseButton, ClickType type);

    @Accessor
    Slot getHoveredSlot();

    @Accessor
    int getGuiLeft();

    @Accessor
    int getGuiTop();

    @Accessor("xSize")
    int getGuiSizeX();

    @Accessor("ySize")
    int getGuiSizeY();
}
