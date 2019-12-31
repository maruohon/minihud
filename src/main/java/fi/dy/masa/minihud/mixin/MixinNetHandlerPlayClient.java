package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.network.play.server.SPacketBlockChange;
import net.minecraft.network.play.server.SPacketChat;
import net.minecraft.network.play.server.SPacketChunkData;
import net.minecraft.network.play.server.SPacketMultiBlockChange;
import net.minecraft.network.play.server.SPacketPlayerListHeaderFooter;
import net.minecraft.network.play.server.SPacketSpawnPosition;
import net.minecraft.network.play.server.SPacketTimeUpdate;
import net.minecraft.util.math.ChunkPos;
import fi.dy.masa.minihud.data.DataStorage;

@Mixin(NetHandlerPlayClient.class)
public abstract class MixinNetHandlerPlayClient
{
    @Inject(method = "handleChat", at = @At("RETURN"))
    private void onChatMessage(SPacketChat packet, CallbackInfo ci)
    {
        DataStorage.getInstance().onChatMessage(packet.getChatComponent());
    }

    @Inject(method = "handleTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(SPacketTimeUpdate packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().getTpsData().onServerTimeUpdate(packetIn.getTotalWorldTime());
    }

    @Inject(method = "handlePlayerListHeaderFooter", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(SPacketPlayerListHeaderFooter packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().getTpsData().parsePlayerListFooterTpsData(packetIn.getFooter());
    }

    @Inject(method = "handleChunkData", at = @At("RETURN"))
    private void markChunkChangedFullChunk(SPacketChunkData packet, CallbackInfo ci)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(packet.getChunkX(), packet.getChunkZ());
    }

    @Inject(method = "handleBlockChange", at = @At("RETURN"))
    private void markChunkChangedBlockChange(SPacketBlockChange packet, CallbackInfo ci)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(packet.getBlockPosition().getX() >> 4, packet.getBlockPosition().getZ() >> 4);
    }

    @Inject(method = "handleMultiBlockChange", at = @At("RETURN"))
    private void markChunkChangedMultiBlockChange(SPacketMultiBlockChange packet, CallbackInfo ci)
    {
        ChunkPos pos = ((IMixinSPacketMultiBlockChange) packet).getChunkPos();
        DataStorage.getInstance().markChunkForHeightmapCheck(pos.x, pos.z);
    }

    @Inject(method = "handleSpawnPosition", at = @At("RETURN"))
    private void onSetSpawn(SPacketSpawnPosition packet, CallbackInfo ci)
    {
        DataStorage.getInstance().setWorldSpawnIfUnknown(packet.getSpawnPos());
    }
}
