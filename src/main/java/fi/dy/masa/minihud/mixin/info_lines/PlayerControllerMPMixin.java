package fi.dy.masa.minihud.mixin.info_lines;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import net.minecraft.client.multiplayer.PlayerControllerMP;
import net.minecraft.util.math.BlockPos;
import fi.dy.masa.minihud.config.InfoLineToggle;
import fi.dy.masa.minihud.data.DataStorage;

@Mixin(PlayerControllerMP.class)
public abstract class PlayerControllerMPMixin
{
    @Inject(method = "onPlayerDestroyBlock",
            at = @At(value = "INVOKE",
                     target = "Lnet/minecraft/block/Block;onPlayerDestroy(" +
                              "Lnet/minecraft/world/World;" +
                              "Lnet/minecraft/util/math/BlockPos;" +
                              "Lnet/minecraft/block/state/IBlockState;)V"))
    private void countBlockBreakingSpeed(BlockPos pos, CallbackInfoReturnable<Boolean> cir)
    {
        if (InfoLineToggle.BLOCK_BREAK_SPEED.getBooleanValue())
        {
            DataStorage.getInstance().onPlayerBlockBreak();
        }
    }
}
