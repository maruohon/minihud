package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import net.minecraft.client.network.NetHandlerPlayClient;
import net.minecraft.network.play.server.SPacketBlockChange;
import net.minecraft.network.play.server.SPacketChat;
import net.minecraft.network.play.server.SPacketChunkData;
import net.minecraft.network.play.server.SPacketMultiBlockChange;
import net.minecraft.network.play.server.SPacketPlayerListHeaderFooter;
import net.minecraft.network.play.server.SPacketTimeUpdate;
import net.minecraft.network.play.server.SPacketUnloadChunk;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.ChunkPos;

@Mixin(NetHandlerPlayClient.class)
public class MixinNetHandlerPlayClient
{
    @Inject(method = "handleChat", at = @At("RETURN"))
    private void onChatMessage(SPacketChat packet, CallbackInfo ci)
    {
        DataStorage.getInstance().onChatMessage(packet.getChatComponent());
    }

    @Inject(method = "handleTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(SPacketTimeUpdate packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().onServerTimeUpdate(packetIn.getTotalWorldTime());
    }

    @Inject(method = "handlePlayerListHeaderFooter", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(SPacketPlayerListHeaderFooter packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().handleCarpetServerTPSData(packetIn.getFooter());
    }

    @Inject(method = "handleChunkData", at = @At("RETURN"))
    private void markChunkChangedFullChunk(SPacketChunkData packet, CallbackInfo ci)
    {
        DataStorage.getInstance().markChunkForHeightmapCheck(packet.getChunkX(), packet.getChunkZ());
    }

    @Inject(method = "handleBlockChange", at = @At("RETURN"))
    private void markChunkChangedBlockChange(SPacketBlockChange packet, CallbackInfo ci)
    {
        BlockPos pos = packet.getPos();
        DataStorage.getInstance().markChunkForHeightmapCheck(pos.getX() >> 4, pos.getZ() >> 4);
    }

    @Inject(method = "handleMultiBlockChange", at = @At("RETURN"))
    private void markChunkChangedMultiBlockChange(SPacketMultiBlockChange packet, CallbackInfo ci)
    {
        ChunkPos pos = ((IMixinSPacketMultiBlockChange) packet).getChunkPos();
        DataStorage.getInstance().markChunkForHeightmapCheck(pos.x, pos.z);
    }

    @Inject(method = "processChunkUnload", at = @At("RETURN"))
    private void unChunkUnload(SPacketUnloadChunk packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().onChunkUnload(packetIn.getX(), packetIn.getZ());
    }
}
