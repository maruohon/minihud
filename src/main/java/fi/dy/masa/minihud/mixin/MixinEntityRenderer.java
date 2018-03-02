package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.RenderEventHandler;
import net.minecraft.client.renderer.EntityRenderer;

@Mixin(EntityRenderer.class)
public class MixinEntityRenderer
{
    @Inject(method = "renderWorldPass(IFJ)V", at = @At(
            value = "INVOKE_STRING",
            target = "Lnet/minecraft/profiler/Profiler;endStartSection(Ljava/lang/String;)V",
            args = "ldc=hand"
        ))
    protected void onRenderWorldLast(int pass, float partialTicks, long finishTimeNano, CallbackInfo ci)
    {
        RenderEventHandler.getInstance().onRenderWorldLast(partialTicks);
    }
}
