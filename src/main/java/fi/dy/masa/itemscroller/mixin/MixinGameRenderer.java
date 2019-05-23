package fi.dy.masa.itemscroller.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.At.Shift;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.itemscroller.event.RenderEventHandler;
import net.minecraft.client.render.GameRenderer;

@Mixin(GameRenderer.class)
public abstract class MixinGameRenderer
{
    @Inject(method = "render(FJZ)V",
            at = @At(value = "INVOKE", shift = Shift.AFTER,
                     target = "Lnet/minecraft/client/gui/screen/Screen;render(IIF)V"))
    private void onDrawScreenPost(float partialTicks, long nanoTime, boolean renderWorldIn, CallbackInfo ci)
    {
        RenderEventHandler.instance().onDrawScreenPost();
    }
}
