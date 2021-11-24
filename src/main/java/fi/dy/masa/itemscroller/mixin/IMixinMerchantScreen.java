package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.gui.screen.ingame.MerchantScreen;

@Mixin(MerchantScreen.class)
public interface IMixinMerchantScreen
{
    @Accessor("selectedIndex")
    int itemscroller_getSelectedMerchantRecipe();
}
