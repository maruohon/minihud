package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.gui.GuiMerchant;

@Mixin(GuiMerchant.class)
public interface IMixinGuiMerchant
{
    @Accessor("selectedMerchantRecipe")
    int getSelectedMerchantRecipe();

    @Accessor("selectedMerchantRecipe")
    void setSelectedMerchantRecipe(int index);
}
