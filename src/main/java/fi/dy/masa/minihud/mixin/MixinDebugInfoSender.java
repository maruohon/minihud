package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.server.network.DebugInfoSender;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import fi.dy.masa.minihud.util.DebugInfoUtils;

@Mixin(DebugInfoSender.class)
public abstract class MixinDebugInfoSender
{
    @Inject(method = "sendNeighborUpdate", at = @At("HEAD"))
    private static void onSendNeighborUpdate(World world, BlockPos pos, CallbackInfo ci)
    {
        DebugInfoUtils.onNeighborUpdate(world, pos);
    }
}
