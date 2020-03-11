package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;

@Mixin(net.minecraft.client.gui.screen.ingame.MerchantScreen.class)
public interface IMixinMerchantScreen
{
    @Accessor("selectedIndex")
    int getSelectedMerchantRecipe();
}
