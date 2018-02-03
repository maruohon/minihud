package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import fi.dy.masa.itemscroller.interfaces.IGuiMerchantAccessor;
import net.minecraft.client.gui.GuiMerchant;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Container;

@Mixin(GuiMerchant.class)
public abstract class MixinGuiMerchant extends GuiContainer implements IGuiMerchantAccessor
{
    @Shadow private int selectedMerchantRecipe;

    public MixinGuiMerchant(Container container)
    {
        super(container);
    }

    @Override
    public int getSelectedMerchantRecipe()
    {
        return this.selectedMerchantRecipe;
    }
}
