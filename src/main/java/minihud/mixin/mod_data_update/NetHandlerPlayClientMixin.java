package minihud.mixin.mod_data_update;

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

import malilib.util.position.BlockPos;
import malilib.util.position.ChunkPos;
import minihud.data.DataStorage;
import minihud.data.MobCapDataHandler;
import minihud.data.TpsDataManager;
import minihud.util.ChatUtils;
import minihud.util.NotificationUtils;

@Mixin(NetHandlerPlayClient.class)
public abstract class NetHandlerPlayClientMixin
{
    @Inject(method = "handleChat", at = @At("RETURN"))
    private void onChatMessage(SPacketChat packet, CallbackInfo ci)
    {
        ChatUtils.onReceiveChatMessage(packet.getChatComponent());
    }

    @Inject(method = "handleTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(SPacketTimeUpdate packetIn, CallbackInfo ci)
    {
        TpsDataManager.INSTANCE.onServerTimeUpdate(packetIn.getTotalWorldTime());
    }

    @Inject(method = "handlePlayerListHeaderFooter", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(SPacketPlayerListHeaderFooter packetIn, CallbackInfo ci)
    {
        TpsDataManager.INSTANCE.parsePlayerListFooterTpsData(packetIn.getFooter());
        MobCapDataHandler.INSTANCE.parsePlayerListFooterMobCapData(packetIn.getFooter());
    }

    @Inject(method = "handleChunkData", at = @At("RETURN"))
    private void markChunkChangedFullChunk(SPacketChunkData packet, CallbackInfo ci)
    {
        NotificationUtils.onChunkData(packet.getChunkX(), packet.getChunkZ(), packet.getTileEntityTags());
    }

    @Inject(method = "handleBlockChange", at = @At("RETURN"))
    private void markChunkChangedBlockChange(SPacketBlockChange packet, CallbackInfo ci)
    {
        NotificationUtils.onBlockChange(BlockPos.of(packet.getBlockPosition()), packet.getBlockState());
    }

    @Inject(method = "handleMultiBlockChange", at = @At("RETURN"))
    private void markChunkChangedMultiBlockChange(SPacketMultiBlockChange packet, CallbackInfo ci)
    {
        net.minecraft.util.math.ChunkPos pos = ((SPacketMultiBlockChangeMixin) packet).minihud_getChunkPos();
        NotificationUtils.onMultiBlockChange(new ChunkPos(pos.x, pos.z), packet.getChangedBlocks());
    }

    @Inject(method = "handleSpawnPosition", at = @At("RETURN"))
    private void onSetSpawn(SPacketSpawnPosition packet, CallbackInfo ci)
    {
        DataStorage.getInstance().setWorldSpawnIfUnknown(BlockPos.of(packet.getSpawnPos()));
    }
}
