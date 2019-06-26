package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.render.GameRenderer;
import fi.dy.masa.minihud.util.DebugInfoUtils;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer
{
    @Inject(method = "renderCenter(FJ)V", at = @At(value = "INVOKE",
            target = "Lnet/minecraft/client/render/debug/DebugRenderer;shouldRender()Z"))
    private void renderDebugRenderers(float partialTicks, long finishTimeNano, CallbackInfo ci)
    {
        DebugInfoUtils.renderVanillaDebug(finishTimeNano);
    }
}
