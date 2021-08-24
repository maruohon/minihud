package fi.dy.masa.minihud.mixin;

import java.util.EnumSet;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import net.minecraft.block.Block;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.Direction;
import net.minecraft.world.World;

@Mixin(World.class)
public abstract class MixinWorld
{
    @Inject(method = "updateNeighbor", at = @At("HEAD"))
    public void onUpdateNeighbor(BlockPos pos, Block sourceBlock, BlockPos neighborPos,CallbackInfo ci)
    {
        DebugInfoUtils.onNeighborUpdate((World) (Object) this, pos);
    }
}
