package fi.dy.masa.minihud.mixin;

import javax.annotation.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.MinecraftClient;
import net.minecraft.client.world.ClientWorld;

@Mixin(MinecraftClient.class)
public abstract class MixinMinecraftClient
{
    @Inject(method = "joinWorld(Lnet/minecraft/client/world/ClientWorld;)V", at = @At("HEAD"))
    private void onLoadWorldPre(@Nullable ClientWorld worldClientIn, CallbackInfo ci)
    {
        if (worldClientIn == null || worldClientIn != (((MinecraftClient) (Object) this).world))
        {
            DataStorage.getInstance().reset();
        }
    }
}
