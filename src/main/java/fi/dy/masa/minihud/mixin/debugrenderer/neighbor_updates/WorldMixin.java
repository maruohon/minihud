package fi.dy.masa.minihud.mixin.debugrenderer.neighbor_updates;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.block.Block;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraft.world.storage.WorldInfo;

import fi.dy.masa.minihud.util.DebugInfoUtils;

@Mixin(World.class)
public abstract class WorldMixin
{
    @Shadow protected WorldInfo worldInfo;

    @Inject(method = "neighborChanged",
            at = @At(value = "INVOKE",
                     target = "Lnet/minecraft/world/World;getBlockState(Lnet/minecraft/util/math/BlockPos;)Lnet/minecraft/block/state/IBlockState;"))
    public void onNeighborChanged(BlockPos pos, Block block, BlockPos fromPos, CallbackInfo ci)
    {
        DebugInfoUtils.onNeighborNotify(pos, this.worldInfo.getWorldTotalTime());
    }
}
