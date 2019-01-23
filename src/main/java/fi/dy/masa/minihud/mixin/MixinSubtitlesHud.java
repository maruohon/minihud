package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.At.Shift;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import com.mojang.blaze3d.platform.GlStateManager;
import fi.dy.masa.minihud.event.RenderHandler;
import net.minecraft.client.gui.hud.SubtitlesHud;

@Mixin(SubtitlesHud.class)
public abstract class MixinSubtitlesHud
{
    @Inject(method = "method_1957", at = @At(
            value = "INVOKE",
            target = "Ljava/util/List;iterator()Ljava/util/Iterator;",
            shift = Shift.AFTER, remap = false))
    private void nudgeSubtitleOverlay(CallbackInfo ci)
    {
        int offset = RenderHandler.getInstance().getSubtitleOffset();

        if (offset != 0)
        {
            GlStateManager.translatef(0, offset, 0);
        }
    }
}
