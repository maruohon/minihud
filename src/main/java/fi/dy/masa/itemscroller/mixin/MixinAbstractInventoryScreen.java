package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.gui.screen.ingame.AbstractInventoryScreen;
import fi.dy.masa.itemscroller.util.InputUtils;

@Mixin(AbstractInventoryScreen.class)
public abstract class MixinAbstractInventoryScreen
{
    @Inject(method = "drawStatusEffects", at = @At("HEAD"), cancellable = true)
    private void preventPotionEffectRendering(CallbackInfo ci)
    {
        if (InputUtils.isRecipeViewOpen())
        {
            ci.cancel();
        }
    }
}
