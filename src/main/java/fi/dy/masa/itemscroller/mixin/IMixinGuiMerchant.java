package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.gui.GuiMerchant;

@Mixin(GuiMerchant.class)
public interface IMixinGuiMerchant
{
    @Accessor
    int getSelectedMerchantRecipe();
}
