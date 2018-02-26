package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.event.InputEventHandler;
import net.minecraft.server.MinecraftServer;

@Mixin(MinecraftServer.class)
public class MixinMinecraftServer
{
    @Inject(method = "tick", at = @At("TAIL"))
    public void onServerTickPost(CallbackInfo ci)
    {
        InputEventHandler.getInstance().onServerTickEnd((MinecraftServer) (Object) this);
    }
}
