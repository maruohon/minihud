package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.renderer.OverlayRendererBeaconRange;

@Mixin(net.minecraft.tileentity.TileEntityBeacon.class)
public abstract class MixinTileEntityBeacon extends net.minecraft.tileentity.TileEntityLockable
{
    @Shadow private int levels;

    private int levelsPre;

    @Inject(method = "setField", at = @At("RETURN"))
    private void onBeaconUpdate(int id, int value, CallbackInfo ci)
    {
        if (id == 0 && value != this.levels)
        {
            OverlayRendererBeaconRange.setNeedsUpdate();
        }
    }

    @Inject(method = "updateSegmentColors", at = @At("HEAD"))
    private void onUpdateSegmentsPre(CallbackInfo ci)
    {
        this.levelsPre = this.levels;
    }

    @Inject(method = "updateSegmentColors", at = @At("RETURN"))
    private void onUpdateSegmentsPost(CallbackInfo ci)
    {
        if (this.levels != this.levelsPre)
        {
            OverlayRendererBeaconRange.setNeedsUpdate();
        }
    }
}
