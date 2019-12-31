package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.profiler.Profiler;
import net.minecraft.world.World;
import net.minecraft.world.WorldProvider;
import net.minecraft.world.storage.ISaveHandler;
import net.minecraft.world.storage.WorldInfo;
import fi.dy.masa.minihud.data.DataStorage;

@Mixin(WorldClient.class)
public abstract class MixinWorldClient extends World
{
    protected MixinWorldClient(ISaveHandler saveHandlerIn, WorldInfo info, WorldProvider providerIn, Profiler profilerIn, boolean client)
    {
        super(saveHandlerIn, info, providerIn, profilerIn, client);
    }

    @Inject(method = "doPreChunk", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/multiplayer/ChunkProviderClient;unloadChunk(II)V"))
    private void onUnloadChunk(int chunkX, int chunkZ, boolean loadChunk, CallbackInfo ci)
    {
        DataStorage.getInstance().onChunkUnload(chunkX, chunkZ);
    }
}
