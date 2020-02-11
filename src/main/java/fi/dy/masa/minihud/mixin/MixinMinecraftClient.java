package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;

@Mixin(net.minecraft.client.MinecraftClient.class)
public abstract class MixinMinecraftClient
{
    @Inject(method = "tick", at = @At("HEAD"))
    private void onClientTickPre(CallbackInfo ci)
    {
        DataStorage.getInstance().onClientTickPre((net.minecraft.client.MinecraftClient) (Object) this);
    }
}
