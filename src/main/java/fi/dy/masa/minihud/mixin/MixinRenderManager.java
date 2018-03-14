package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import fi.dy.masa.minihud.config.ConfigsGeneric;
import net.minecraft.client.renderer.culling.ICamera;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.Entity;
import net.minecraft.entity.item.EntityFallingBlock;

@Mixin(RenderManager.class)
public class MixinRenderManager
{
    @Inject(method = "shouldRender", at = @At("HEAD"), cancellable = true)
    private void onShouldRender(Entity entityIn, ICamera camera, double camX, double camY, double camZ, CallbackInfoReturnable<Boolean> ci)
    {
        if (ConfigsGeneric.TWEAK_NO_FALLING_BLOCK_RENDER.getBooleanValue() && entityIn instanceof EntityFallingBlock)
        {
            ci.setReturnValue(false);
            ci.cancel();
        }
    }
}
