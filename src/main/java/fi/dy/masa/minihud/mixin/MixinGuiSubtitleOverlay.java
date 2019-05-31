package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.At.Shift;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.RenderHandler;
import net.minecraft.client.gui.Gui;
import net.minecraft.client.gui.GuiSubtitleOverlay;
import net.minecraft.client.renderer.GlStateManager;

@Mixin(GuiSubtitleOverlay.class)
public abstract class MixinGuiSubtitleOverlay extends Gui
{
    @Inject(method = "render", at = @At(
            value = "INVOKE",
            target = "Lnet/minecraft/client/renderer/GlStateManager;blendFuncSeparate(" +
            "Lnet/minecraft/client/renderer/GlStateManager$SourceFactor;" +
            "Lnet/minecraft/client/renderer/GlStateManager$DestFactor;" +
            "Lnet/minecraft/client/renderer/GlStateManager$SourceFactor;" +
            "Lnet/minecraft/client/renderer/GlStateManager$DestFactor;)V",
            shift = Shift.AFTER))
    private void nudgeSubtitleOverlay(CallbackInfo ci)
    {
        int offset = RenderHandler.getInstance().getSubtitleOffset();

        if (offset != 0)
        {
            GlStateManager.translatef(0, offset, 0);
        }
    }
}
