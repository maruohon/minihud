package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import fi.dy.masa.minihud.util.DataStorage;

@Mixin(net.minecraft.client.multiplayer.PlayerControllerMP.class)
public abstract class MixinPlayerControllerMP
{
    @Shadow @Final private net.minecraft.client.Minecraft mc;

    @Inject(method = "onPlayerDestroyBlock", at = @At(value = "INVOKE",
                target = "Lnet/minecraft/block/Block;onPlayerDestroy(" +
                         "Lnet/minecraft/world/World;" +
                         "Lnet/minecraft/util/math/BlockPos;" +
                         "Lnet/minecraft/block/state/IBlockState;)V"))
    private void countBlockBreakingSpeed(net.minecraft.util.math.BlockPos pos, CallbackInfoReturnable<Boolean> cir)
    {
        DataStorage.getInstance().onPlayerBlockBreak(this.mc);
    }
}
