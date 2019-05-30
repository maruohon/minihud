package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.util.InputUtils;
import net.minecraft.client.renderer.InventoryEffectRenderer;

@Mixin(InventoryEffectRenderer.class)
public abstract class MixinInventoryEffectRenderer
{
    @Inject(method = "drawActivePotionEffects", at = @At("HEAD"), cancellable = true)
    private void preventPotionEffectRendering(CallbackInfo ci)
    {
        if (InputUtils.isRecipeViewOpen())
        {
            ci.cancel();
        }
    }
}
