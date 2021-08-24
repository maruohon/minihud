package fi.dy.masa.minihud.mixin;

import fi.dy.masa.minihud.util.DebugInfoUtils;
import net.minecraft.block.BlockState;
import net.minecraft.block.FacingBlock;
import net.minecraft.block.ObserverBlock;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ObserverBlock.class)
public class MixinObserverBlock {

    @Inject(method = "updateNeighbors", at = @At("HEAD"))
    public void onNotifyNeighbors(World world, BlockPos pos, BlockState state, CallbackInfo ci)
    {
        DebugInfoUtils.onNeighborNotify(world, pos, EnumSet.of(state.get(FacingBlock.FACING).getOpposite()));
    }
}
