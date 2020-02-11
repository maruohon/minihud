package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;
import fi.dy.masa.minihud.util.DataStorage;
import fi.dy.masa.minihud.util.NotificationUtils;

@Mixin(net.minecraft.client.network.ClientPlayNetworkHandler.class)
public abstract class MixinClientPlayNetworkHandler
{
    @Inject(method = "onBlockUpdate", at = @At("RETURN"))
    private void markChunkChangedBlockChange(net.minecraft.network.packet.s2c.play.BlockUpdateS2CPacket packet, CallbackInfo ci)
    {
        NotificationUtils.onBlockChange(packet.getPos(), packet.getState());
    }

    @Inject(method = "onChunkData", at = @At("RETURN"))
    private void markChunkChangedFullChunk(net.minecraft.network.packet.s2c.play.ChunkDataS2CPacket packet, CallbackInfo ci)
    {
        NotificationUtils.onChunkData(packet.getX(), packet.getZ(), packet.getChunkData());
    }

    @Inject(method = "onChunkDeltaUpdate", at = @At("RETURN"))
    private void markChunkChangedMultiBlockChange(net.minecraft.network.packet.s2c.play.ChunkDeltaUpdateS2CPacket packet, CallbackInfo ci)
    {
        net.minecraft.util.math.ChunkSectionPos pos = ((IMixinChunkDeltaUpdateS2CPacket) packet).minihud_getChunkSectionPos();
        NotificationUtils.onMultiBlockChange(pos, packet);
    }

    @Inject(method = "onGameMessage", at = @At("RETURN"))
    private void onGameMessage(net.minecraft.network.packet.s2c.play.GameMessageS2CPacket packet, CallbackInfo ci)
    {
        DataStorage.getInstance().onChatMessage(packet.getMessage());
    }

    @Inject(method = "onPlayerListHeader", at = @At("RETURN"))
    private void onHandlePlayerListHeaderFooter(net.minecraft.network.packet.s2c.play.PlayerListHeaderS2CPacket packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().handleCarpetServerTPSData(packetIn.getFooter());
    }

    @Inject(method = "onWorldTimeUpdate", at = @At("RETURN"))
    private void onTimeUpdate(net.minecraft.network.packet.s2c.play.WorldTimeUpdateS2CPacket packetIn, CallbackInfo ci)
    {
        DataStorage.getInstance().onServerTimeUpdate(packetIn.getTime());
    }

    @Inject(method = "onPlayerSpawnPosition", at = @At("RETURN"))
    private void onSetSpawn(net.minecraft.network.packet.s2c.play.PlayerSpawnPositionS2CPacket packet, CallbackInfo ci)
    {
        DataStorage.getInstance().setWorldSpawnIfUnknown(packet.getPos());
    }
}
