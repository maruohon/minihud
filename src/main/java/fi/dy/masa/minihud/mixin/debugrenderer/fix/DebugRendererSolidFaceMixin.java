package fi.dy.masa.minihud.mixin.debugrenderer.fix;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.client.renderer.debug.DebugRendererSolidFace;

import fi.dy.masa.minihud.event.RenderHandler;

@Mixin(DebugRendererSolidFace.class)
public abstract class DebugRendererSolidFaceMixin
{
    @Inject(method = "render", at = @At("HEAD"))
    public void fixDebugRendererState(CallbackInfo ci)
    {
        RenderHandler.fixDebugRendererState();
    }
}
