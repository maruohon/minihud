package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.At.Shift;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import com.mojang.blaze3d.systems.RenderSystem;
import net.minecraft.client.gui.hud.SubtitlesHud;
import fi.dy.masa.minihud.event.RenderHandler;

@Mixin(SubtitlesHud.class)
public abstract class MixinSubtitlesHud
{
    @Inject(method = "render", at = @At(
            value = "INVOKE",
            target = "Lcom/mojang/blaze3d/systems/RenderSystem;enableBlend()V",
            shift = Shift.AFTER, ordinal = 0))
    private void nudgeSubtitleOverlay(CallbackInfo ci)
    {
        int offset = RenderHandler.getInstance().getSubtitleOffset();

        if (offset != 0)
        {
            RenderSystem.translatef(0, offset, 0);
        }
    }
}
