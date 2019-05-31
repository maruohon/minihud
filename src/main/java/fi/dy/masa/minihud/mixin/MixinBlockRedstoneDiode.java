package fi.dy.masa.minihud.mixin;

import java.util.EnumSet;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockHorizontal;
import net.minecraft.block.BlockRedstoneDiode;
import net.minecraft.block.state.IBlockState;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(BlockRedstoneDiode.class)
public abstract class MixinBlockRedstoneDiode extends BlockHorizontal
{
    protected MixinBlockRedstoneDiode(Block.Properties builder)
    {
        super(builder);
    }

    @Inject(method = "notifyNeighbors", at = @At("HEAD"))
    public void onNotifyNeighbors(World world, BlockPos pos, IBlockState state, CallbackInfo ci)
    {
        EnumSet<EnumFacing> set = EnumSet.of(state.get(BlockHorizontal.HORIZONTAL_FACING).getOpposite());
        DebugInfoUtils.onNeighborNotify(world, pos, set);
    }
}
