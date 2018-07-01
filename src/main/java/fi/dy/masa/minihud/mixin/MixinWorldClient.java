package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.multiplayer.WorldClient;

@Mixin(WorldClient.class)
public class MixinWorldClient
{
    @Inject(method = "doPreChunk", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/multiplayer/ChunkProviderClient;unloadChunk(II)V"))
    private void onUnloadChunk(int chunkX, int chunkZ, boolean loadChunk, CallbackInfo ci)
    {
        DataStorage.getInstance().onChunkUnload(chunkX, chunkZ);
    }
}
