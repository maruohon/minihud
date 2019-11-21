package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.render.debug.CollisionDebugRenderer;
import fi.dy.masa.minihud.event.RenderHandler;

@Mixin(CollisionDebugRenderer.class)
public abstract class MixinCollisionDebugRenderer
{
    @Inject(method = "render", at = @At("HEAD"))
    public void fixDebugRendererState(CallbackInfo ci)
    {
        RenderHandler.fixDebugRendererState();
    }
}
