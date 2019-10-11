package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.render.debug.DebugRenderer;
import fi.dy.masa.minihud.util.DebugInfoUtils;

@Mixin(DebugRenderer.class)
public abstract class MixinDebugRenderer
{
    @Inject(method = "method_23099", at = @At("RETURN"))
    private void renderDebugRenderers(long finishTimeNano, CallbackInfo ci)
    {
        DebugInfoUtils.renderVanillaDebug(finishTimeNano);
    }
}
