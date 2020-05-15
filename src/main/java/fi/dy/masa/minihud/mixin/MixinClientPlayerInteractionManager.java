package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;
import fi.dy.masa.minihud.util.DataStorage;

@Mixin(net.minecraft.client.network.ClientPlayerInteractionManager.class)
public abstract class MixinClientPlayerInteractionManager
{
    @Shadow @Final private net.minecraft.client.MinecraftClient client;

    @Inject(method = "breakBlock", at = @At(value = "INVOKE",
                target = "Lnet/minecraft/block/Block;onBroken(" +
                         "Lnet/minecraft/world/WorldAccess;" +
                         "Lnet/minecraft/util/math/BlockPos;" +
                         "Lnet/minecraft/block/BlockState;)V"))
    private void countBlockBreakingSpeed(net.minecraft.util.math.BlockPos pos, CallbackInfoReturnable<Boolean> cir)
    {
        DataStorage.getInstance().onPlayerBlockBreak(this.client);
    }
}
