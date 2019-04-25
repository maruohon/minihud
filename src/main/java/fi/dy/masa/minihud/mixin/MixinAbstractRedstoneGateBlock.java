package fi.dy.masa.minihud.mixin;

import java.util.EnumSet;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import net.minecraft.block.AbstractRedstoneGateBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.HorizontalFacingBlock;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;

@Mixin(AbstractRedstoneGateBlock.class)
public abstract class MixinAbstractRedstoneGateBlock extends HorizontalFacingBlock
{
    protected MixinAbstractRedstoneGateBlock(Block.Settings builder)
    {
        super(builder);
    }

    @Inject(method = "updateTarget", at = @At("HEAD"))
    public void onNotifyNeighbors(World world, BlockPos pos, BlockState state, CallbackInfo ci)
    {
        EnumSet<Direction> set = EnumSet.of(state.get(HorizontalFacingBlock.FACING).getOpposite());
        DebugInfoUtils.onNeighborNotify(world, pos, set);
    }
}
