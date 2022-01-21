package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.render.VertexConsumerProvider;
import net.minecraft.client.render.debug.DebugRenderer;
import net.minecraft.client.util.math.MatrixStack;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.DebugInfoUtils;

@Mixin(DebugRenderer.class)
public abstract class MixinDebugRenderer
{
    @Inject(method = "render", at = @At("RETURN"))
    private void renderDebugRenderers(MatrixStack matrixStack, VertexConsumerProvider.Immediate vtx,
            double cameraX, double cameraY, double cameraZ, CallbackInfo ci)
    {
        if (Configs.Generic.MAIN_RENDERING_TOGGLE.getBooleanValue())
        {
            DebugInfoUtils.renderVanillaDebug(matrixStack, vtx, cameraX, cameraY, cameraZ);
        }
    }
}
