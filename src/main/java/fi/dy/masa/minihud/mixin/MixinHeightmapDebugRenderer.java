package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.RenderHandler;
import net.minecraft.client.render.debug.HeightmapDebugRenderer;

@Mixin(HeightmapDebugRenderer.class)
public abstract class MixinHeightmapDebugRenderer
{
    @Inject(method = "render", at = @At("HEAD"))
    public void fixDebugRendererState(CallbackInfo ci)
    {
        RenderHandler.fixDebugRendererState();
    }
}
