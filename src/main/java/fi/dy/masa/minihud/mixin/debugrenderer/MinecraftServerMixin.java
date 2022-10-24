package fi.dy.masa.minihud.mixin.debugrenderer;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

import net.minecraft.server.MinecraftServer;

import fi.dy.masa.minihud.util.DebugInfoUtils;

@Mixin(MinecraftServer.class)
public abstract class MinecraftServerMixin
{
    @Inject(method = "tick", at = @At("TAIL"))
    public void onServerTickPost(CallbackInfo ci)
    {
        DebugInfoUtils.updateDebugRenderersOnServerTickEnd((MinecraftServer) (Object) this);
    }
}
