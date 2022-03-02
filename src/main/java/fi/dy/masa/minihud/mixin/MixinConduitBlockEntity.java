package fi.dy.masa.minihud.mixin;

import java.util.List;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.block.BlockState;
import net.minecraft.block.entity.ConduitBlockEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.renderer.OverlayRendererConduitRange;
import fi.dy.masa.minihud.util.ConduitExtra;

@Mixin(ConduitBlockEntity.class)
public abstract class MixinConduitBlockEntity implements ConduitExtra
{
    @Shadow @Final private List<BlockPos> activatingBlocks;
    private int minihud_activatingBlockCount;

    @Override
    public int getCurrentActivatingBlockCount()
    {
        return this.activatingBlocks.size();
    }

    @Override
    public int getStoredActivatingBlockCount()
    {
        return this.minihud_activatingBlockCount;
    }

    @Override
    public void setActivatingBlockCount(int count)
    {
        this.minihud_activatingBlockCount = count;
    }

    @Inject(method = "clientTick",
            at = @At(value = "INVOKE",
                    target = "Lnet/minecraft/block/entity/ConduitBlockEntity;openEye(Lnet/minecraft/block/entity/ConduitBlockEntity;Ljava/util/List;)V"))
    private static void minihud_postActiveBlockScan(World world, BlockPos pos, BlockState state,
                                                    ConduitBlockEntity blockEntity, CallbackInfo ci)
    {
        if (RendererToggle.OVERLAY_CONDUIT_RANGE.getBooleanValue())
        {
            int count = ((ConduitExtra) blockEntity).getCurrentActivatingBlockCount();
            int countBefore = ((ConduitExtra) blockEntity).getStoredActivatingBlockCount();

            if (count != countBefore)
            {
                OverlayRendererConduitRange.INSTANCE.onBlockStatusChange(pos);
                ((ConduitExtra) blockEntity).setActivatingBlockCount(count);
            }
        }
    }
}
