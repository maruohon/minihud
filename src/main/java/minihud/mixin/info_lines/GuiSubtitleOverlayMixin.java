package minihud.mixin.info_lines;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.At.Shift;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.client.gui.Gui;
import net.minecraft.client.gui.GuiSubtitleOverlay;
import net.minecraft.client.renderer.GlStateManager;

import minihud.config.Configs;
import minihud.event.RenderHandler;

@Mixin(GuiSubtitleOverlay.class)
public abstract class GuiSubtitleOverlayMixin extends Gui
{
    @Inject(method = "renderSubtitles", at = @At(
            value = "INVOKE",
            target = "Lnet/minecraft/client/renderer/GlStateManager;tryBlendFuncSeparate(" +
            "Lnet/minecraft/client/renderer/GlStateManager$SourceFactor;" +
            "Lnet/minecraft/client/renderer/GlStateManager$DestFactor;" +
            "Lnet/minecraft/client/renderer/GlStateManager$SourceFactor;" +
            "Lnet/minecraft/client/renderer/GlStateManager$DestFactor;)V",
            shift = Shift.AFTER))
    private void nudgeSubtitleOverlay(CallbackInfo ci)
    {
        if (Configs.Generic.OFFSET_SUBTITLE_HUD.getBooleanValue())
        {
            int offset = RenderHandler.INSTANCE.getSubtitleOffset();

            if (offset != 0)
            {
                GlStateManager.translate(0, offset, 0);
            }
        }
    }
}
