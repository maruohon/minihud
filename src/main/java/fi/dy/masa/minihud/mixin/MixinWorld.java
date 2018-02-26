package fi.dy.masa.minihud.mixin;

import java.util.EnumSet;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.InputEventHandler;
import net.minecraft.block.Block;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

@Mixin(World.class)
public class MixinWorld
{
    @Inject(method = "notifyNeighborsOfStateChange", at = @At("HEAD"))
    public void onNotifyNeighborsOfStateChange(BlockPos pos, Block blockType, boolean updateObservers, CallbackInfo ci)
    {
        EnumSet<EnumFacing> set = EnumSet.allOf(EnumFacing.class);
        InputEventHandler.getInstance().onNeighborNotify((World) (Object) this, pos, set);
    }

    @Inject(method = "notifyNeighborsOfStateExcept", at = @At("HEAD"))
    public void onNotifyNeighborsOfStateExcept(BlockPos pos, Block blockType, EnumFacing skipSide, CallbackInfo ci)
    {
        EnumSet<EnumFacing> set = EnumSet.allOf(EnumFacing.class);
        set.remove(skipSide);
        InputEventHandler.getInstance().onNeighborNotify((World) (Object) this, pos, set);
    }
}
