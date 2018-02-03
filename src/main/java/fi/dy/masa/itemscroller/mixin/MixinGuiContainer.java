package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import fi.dy.masa.itemscroller.interfaces.IGuiContainerAccessor;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Slot;

@Mixin(GuiContainer.class)
public class MixinGuiContainer extends GuiScreen implements IGuiContainerAccessor
{
    @Shadow private Slot hoveredSlot;
    @Shadow protected int xSize;
    @Shadow protected int ySize;
    @Shadow protected int guiLeft;
    @Shadow protected int guiTop;;

    @Shadow private Slot getSlotAtPosition(int x, int y) { return null; }

    @Override
    public Slot getSlotAt(int x, int y)
    {
        return this.getSlotAtPosition(x, y);
    }

    @Override
    public Slot getHoveredSlot()
    {
        return this.hoveredSlot;
    }

    @Override
    public int getGuiLeft()
    {
        return this.guiLeft;
    }

    @Override
    public int getGuiTop()
    {
        return this.guiTop;
    }

    @Override
    public int getGuiSizeX()
    {
        return this.xSize;
    }

    @Override
    public int getGuiSizeY()
    {
        return this.ySize;
    }
}
