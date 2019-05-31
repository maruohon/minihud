package fi.dy.masa.minihud.mixin;

import java.util.EnumSet;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import net.minecraft.block.Block;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(World.class)
public abstract class MixinWorld
{
    @Inject(method = "notifyNeighborsOfStateChange", at = @At("HEAD"))
    public void onNotifyNeighborsOfStateChange(BlockPos pos, Block blockType, CallbackInfo ci)
    {
        EnumSet<EnumFacing> set = EnumSet.allOf(EnumFacing.class);
        DebugInfoUtils.onNeighborNotify((World) (Object) this, pos, set);
    }

    @Inject(method = "notifyNeighborsOfStateExcept", at = @At("HEAD"))
    public void onNotifyNeighborsOfStateExcept(BlockPos pos, Block blockType, EnumFacing skipSide, CallbackInfo ci)
    {
        EnumSet<EnumFacing> set = EnumSet.allOf(EnumFacing.class);
        set.remove(skipSide);
        DebugInfoUtils.onNeighborNotify((World) (Object) this, pos, set);
    }
}
