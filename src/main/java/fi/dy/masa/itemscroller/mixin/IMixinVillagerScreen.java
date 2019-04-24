package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.client.gui.container.VillagerScreen;

@Mixin(VillagerScreen.class)
public interface IMixinVillagerScreen
{
    @Accessor("field_19161")
    int getSelectedMerchantRecipe();
}
