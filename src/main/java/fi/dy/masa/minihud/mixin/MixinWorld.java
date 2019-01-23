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
    @Inject(method = "updateNeighborsAlways", at = @At("HEAD"))
    public void onUpdateNeighborsAlways(BlockPos pos, Block blockType, CallbackInfo ci)
    {
        EnumSet<Direction> set = EnumSet.allOf(Direction.class);
        DebugInfoUtils.onNeighborNotify((World) (Object) this, pos, set);
    }

    @Inject(method = "updateNeighborsExcept", at = @At("HEAD"))
    public void onUpdateNeighborsExcept(BlockPos pos, Block blockType, Direction skipSide, CallbackInfo ci)
    {
        EnumSet<Direction> set = EnumSet.allOf(Direction.class);
        set.remove(skipSide);
        DebugInfoUtils.onNeighborNotify((World) (Object) this, pos, set);
    }
}
